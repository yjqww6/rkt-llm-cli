#lang typed/racket/base/shallow
(require "main.rkt"
         "types.rkt"
         racket/list
         racket/match
         racket/string)
(provide chat-template chatml skip-cot-tokens)

(define (split-prefill [messages : History])
  (match/values
   (split-at-right messages 1)
   [(h (list (struct* Msg ([role "assistant"] [content prefill]))))
    (values h prefill)]
   [(_ _)
    (values messages #f)]))

(define (inject-system-to-user [messages : History]
                               [join : (-> String String String)
                                     (λ (a b) (string-append a "\n\n" b))])
  (match messages
    [(list* (struct* Msg ([role "system"] [content system]))
            (and (struct* Msg ([role "user"] [content content])) user)
            rest)
     (cons (struct-copy Msg user [content (join system content)])
           rest)]
    [(cons (struct* Msg ([role "system"])) _)
     (error 'inject-system-to-user "system must be followed by user")]
    [_ messages]))

(define (chatml [messages : History] [o : Options]) : String
  (define-values (his prefill) (split-prefill messages))
  (define s (open-output-string))
  (for ([msg (in-list his)])
    (match-define (struct* Msg ([role role] [content content])) msg)
    (write-string (format "<|im_start|>~a\n" role) s)
    (write-string content s)
    (write-string "<|im_end|>\n" s))
  (write-string "<|im_start|>assistant\n" s)
  (when prefill
    (write-string prefill s))
  (get-output-string s))

(define (gemma [messages : History] [o : Options]) : String
  (define-values (h prefill) (split-prefill messages))
  (define s (open-output-string))
  (for ([msg (in-list (inject-system-to-user h))])
    (match-define (struct* Msg ([role role] [content content])) msg)
    (define mapped-role (if (string=? role "assistant") "model" role))
    (write-string (format "<start_of_turn>~a\n" mapped-role) s)
    (write-string content s)
    (write-string "<end_of_turn>\n" s))
  (write-string "<start_of_turn>model\n" s)
  (when prefill
    (write-string prefill s))
  (get-output-string s))

(define (tool-call->jsexpr [tc : ToolCall]) : JSExpr
  (match-define (struct* ToolCall ([name name] [arguments arguments] [id id])) tc)
  (hasheq 'name name
          'arguments (string->jsexpr arguments)
          'id id))

;;; mistral-v7-tekken
(define (mistral [messages : History] [o : Options]) : String
  (define-values (h prefill) (split-prefill messages))
  (define s (open-output-string))
  (define (put [str : String]) (write-string str s))
  (define len (length h))
  (for ([msg (in-list h)]
        [i (in-naturals 1)])
    (match msg
      [(struct* Msg ([role "system"] [content content]))
       (put "[SYSTEM_PROMPT]")
       (put content)
       (put "[/SYSTEM_PROMPT]")]
      [(struct* Msg ([role "user"] [content content]))
       (when (= i len)
         (define tools (Options-tools o))
         (unless (null? tools)
           (put "[AVAILABLE_TOOLS]")
           (put (jsexpr->string (map Tool-desc tools)))
           (put "[/AVAILABLE_TOOLS]")))
       (put "[INST]")
       (put content)
       (put "[/INST]")]
      [(struct* Msg ([role "assistant"] [content content] [tool-calls tool-calls]))
       (put (string-trim content #:left? #f))
       (unless (null? tool-calls)
         (put "[TOOL_CALLS]")
         (put (jsexpr->string (map tool-call->jsexpr tool-calls))))]
      [(struct* Msg ([role "tool"] [content content] [tool-call-id (? string? tool-call-id)]))
       (put "[TOOL_RESULTS]")
       (put tool-call-id)
       (put "[TOOL_CONTENT]")
       (put content)
       (put "[/TOOL_RESULTS]")]))
  (when prefill
    (put prefill))
  (get-output-string s))

(define (skip-cot-tokens [msgs : History] #:sep [sep : String "</think>"]) : History
  (define end (length msgs))
  (for/list ([m (in-list msgs)]
             [i (in-naturals 1)])
    (match m
      [(struct* Msg ([role "assistant"] [content content]))
       #:when (< i end)
       #:when (string-contains? content sep)
       (define new-content 
         (string-trim (last (string-split content sep)) "\n"
                      #:right? #f #:repeat? #t))
       (struct-copy Msg m [content new-content])]
      [else m])))

(define (chat-template [name : String] #:skip-cot? [skip-cot? : Boolean #t]) : ChatTemplate
  (define tpl
    (match name
      ["chatml" chatml]
      ["gemma" gemma]
      ["mistral" mistral]))
  (cond
    [(not skip-cot?) tpl]
    [else
     (λ (m o) (tpl (skip-cot-tokens m) o))]))