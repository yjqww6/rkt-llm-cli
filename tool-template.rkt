#lang typed/racket/base/shallow
(require racket/string
         racket/match
         "private/main.rkt"
         "private/types.rkt")
(provide (all-defined-out))

(define (match-all [rx : PRegexp] [str : String]) : (Listof (Listof String))
  (define matches (regexp-match* rx str))
  (cond
    [(not matches) '()]
    [else
     (for/list ([m (in-list matches)])
       (match-define (cons _ r) (regexp-match rx m))
       (assert (andmap string? r))
       r)]))

(define (make-nous-system-template [tools : (Listof JSExpr)] [system : (Option String)])
  (define prefix #<<TPL
# Tools

You may call one or more functions to assist with the user query.

You are provided with function signatures within <tools></tools> XML tags:
<tools>

TPL
    )
  (define suffix #<<TPL

</tools>

For each function call, return a json object with function name and arguments within <tool_call></tool_call> XML tags:
<tool_call>
{"name": <function-name>, "arguments": <args-json-object>}
</tool_call>
TPL
    )
  (cond
    [(null? tools) system]
    [else
     (string-append
      (if system system "")
      (if system "\n\n" "")
      prefix
      (string-join (map jsexpr->string tools) "\n")
      suffix)]))

(define (parse-nous-toolcall [response : String]) : (Listof ToolCall)
  (for/list ([call (in-list (match-all #px"<tool_call>\\s*(.*?)\\s*</tool_call>" response))])
    (match (string->jsexpr (car call))
      [(hash* ['name (? string? name)] ['arguments arguments])
       (ToolCall name (jsexpr->string arguments) "")])))

(define (make-nous-response [response : String])
  (string-append "<tool_response>" response "</tool_response>"))

(define (make-step-system-template [tools : (Listof JSExpr)] [system : (Option String)])
  (define prefix #<<TPL
# Tools

You have access to the following functions in JSONSchema format:


<tools>
TPL
    )
  (define suffix #<<TPL

</tools>

If you choose to call a function ONLY reply in the following format with NO suffix:

<tool_call>
<function=example_function_name>
<parameter=example_parameter_1>
value_1
</parameter>
<parameter=example_parameter_2>
This is the value for the second parameter
that can span
multiple lines
</parameter>
</function>
</tool_call>

<IMPORTANT>
Reminder:
- Function calls MUST follow the specified format: an inner <function=...>
...
</function> block must be nested within <tool_call>
...
</tool_call> XML tags
- Required parameters MUST be specified
</IMPORTANT>
TPL
    )
  (cond
    [(null? tools) system]
    [else
     (string-append
      (if system system "")
      (if system "\n\n" "")
      prefix
      (string-join (map jsexpr->string tools) "\n" #:before-first "\n")
      suffix)]))

(define (parse-step-response [response : String] [repair : (String Symbol -> (U 'str 'json))]) : (Listof ToolCall)
  (match (regexp-match #px"<tool_call>\\s*<function=([^>]+)>(.*?)</function>\\s*</tool_call>" response)
    [(list _ function-name inner-content)
     #:when (and function-name inner-content)
     (define parameters
       (for/fold ([h : (HashTable Symbol JSExpr) (hasheq)])
                 ([param (in-list (match-all #px"<parameter=([^>]+)>\n(.*?)\n</parameter>" inner-content))])
         (match-define (list p a) param)
         (define k (string->symbol p))
         (define v
           (case (repair function-name k)
             [(str) a]
             [(json) (with-handlers ([exn:fail:read? (λ (e) a)])
                       (string->jsexpr a))]))
         (hash-set h k v)))
     (list (ToolCall function-name (jsexpr->string parameters) ""))]
    [_ '()]))

(define (make-step-response [response : String])
  (string-append "<tool_response>\n" response "\n</tool_response>"))

(define (parse-mistral-toolcall [response : String])
  (define (submatch [px : Regexp]) : (Listof ToolCall)
    (match (regexp-match px response)
      [(list _ (? string? tcs))
       (with-handlers* ([exn:fail:read? (λ (_) '())])
         (let/ec k : (Listof ToolCall)
           (match (string->jsexpr tcs)
             [(list tc ...)
              (map (λ ([tc : JSExpr]) : ToolCall
                     (match tc
                       [(hash* ['name (? string? name)] ['arguments arguments])
                        (ToolCall name (jsexpr->string arguments) "")]
                       [_ (k '())]))
                   tc)]
             [_ '()])))]
      [_ '()]))
  (define sp (submatch #px"\\[TOOL_CALLS\\]\\s*(.*?)\\s*$"))
  (if (null? sp)
      (submatch #px"\\s*(\\[\\{.*?)\\s*$")
      sp))
