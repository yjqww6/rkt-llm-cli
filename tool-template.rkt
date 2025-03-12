#lang typed/racket/base/shallow
(require racket/string
         racket/match
         "private/main.rkt"
         "private/types.rkt")
(provide (all-defined-out))

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
  (match (regexp-match* #px"<tool_call>\\s*(.*?)\\s*</tool_call>" response #:match-select cadr)
    [(list call ...)
     (map
      (λ ([call : String])
        (match (string->jsexpr call)
          [(hash* ['name (? string? name)] ['arguments arguments])
           (ToolCall name (jsexpr->string arguments) "")]))
      call)]
    [else '()]))

(define (make-nous-response [response : String])
  (string-append "<tool_response>" response "</tool_response>"))