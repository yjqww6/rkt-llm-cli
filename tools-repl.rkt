#lang typed/racket/base/shallow
(require "main.rkt"
         "tool-template.rkt"
         "private/types.rkt"
         "private/main.rkt"
         racket/match
         racket/list)
(provide current-tool-callback default-tool-callback
         execute tool-repl-prompt make-auto-execute-chat
         with-nous-tools with-mistral-tools)

(define-type ToolCallback (-> String String (Option String)))
(define current-tool-callback
  (make-parameter (ann (λ (sym tcs) (error 'tool-callback)) ToolCallback)))
(define current-tool-parser
  (make-parameter (ann (λ (s) '()) (-> String (Listof ToolCall)))))

(define (tool-repl-prompt)
  (string-append "TOOL:" (default-repl-prompt)))

(define (execute) : Void
  (define (calling [tcs : (Listof ToolCall)])
    (define tool-args : (Listof (HashTable Symbol String))
      (map
       (λ ([tc : ToolCall])
         (match-define (ToolCall name args id) tc)
         (hasheq 'name name 'arguments args))
       tcs))
    (let/ec k : Void
      (define tool-resps : (Listof String)
        (for/list ([arg (in-list tool-args)])
          (match-define (hash* ['name (? string? name)] ['arguments arguments]) arg)
          (call/color
           'blue
           (λ () (printf "Executing ~a: ~a" name arguments)))
          (define r
            ((current-tool-callback) name arguments))
          (unless r (k (void)))
          r))
      (define tool-msgs
        (map (λ ([rsp : String] [tc : ToolCall])
               (make-tool rsp (ToolCall-id tc)))
             tool-resps tcs))
      ((current-chat) (cons 'result tool-msgs))))
  (match (current-history)
    [(list _ ... (struct* Msg ([role "assistant"] [tool-calls tcs])))
     #:when (not (null? tcs))
     (calling tcs)]
    [(list _ ... (struct* Msg ([role "assistant"]
                               [content (app (current-tool-parser) tcs)])))
     #:when (not (null? tcs))
     (calling tcs)]
    [else (void)]))

(define (make-auto-execute-chat [chat : Chat])
  (λ ([s : Interactive])
    (chat s)
    (execute)))

(: default-tool-callback ToolCallback)
(define (default-tool-callback name arg)
  (cond
    [(memf (λ ([t : Tool]) (string=? name (Tool-name t))) (current-tools))
     =>
     (λ (tools)
       ((Tool-callback (car tools)) arg))]
    [else (error 'default-tool-callback "~a not found" name)]))

(define (tool->user [msg : Msg])
  (if (string=? (Msg-role msg) "tool")
      (struct-copy Msg msg [role (ann "user" Role)])
      msg))

(define (map-system [f : (-> (Option String) (Option String))] [h : History]) : History
  (: s (Option String))
  (define-values (s r)
    (match h
      [(cons (struct* Msg ([role "system"] [content content])) r) (values content r)]
      [_ (values #f h)]))
  (cond
    [(f s) => (λ (sys) (cons (make-system sys) r))]
    [else r]))

(define (with-nous-tools [repl : (-> Void)])
  (define old-callback (current-tool-callback))
  (define (system-rewrite [s : (Option String)]) : (Option String)
    (make-nous-system-template (map Tool-desc (current-tools)) s))
  (parameterize ([current-tool-callback (λ ([name : String] [arg : String])
                                          (define r (old-callback name arg))
                                          (cond
                                            [(not r) #f]
                                            [else
                                             (make-nous-response r)]))]
                 [current-tool-parser parse-nous-toolcall]
                 [current-interactive-hooks (cons
                                             (ann
                                              (λ (i o)
                                                (values i (struct-copy Options o [tools '()])))
                                              InteractiveHook)
                                             (current-interactive-hooks))]
                 [current-messages-preprocessors (cons
                                                  (compose1
                                                   (λ ([s : History]) (map tool->user s))
                                                   (λ ([s : History]) (map-system system-rewrite s)))
                                                  (current-messages-preprocessors))])
    (repl)))

(define (with-mistral-tools [repl : (-> Void)])
  (define (post [m : Msg])
    (match m
      [(struct* Msg ([role "assistant"] [content (app parse-mistral-toolcall tcs)]))
       #:when (not (null? tcs))
       (struct-copy Msg m
                    [content ""]
                    [tool-calls (map (λ ([tc : ToolCall])
                                       (struct-copy ToolCall tc
                                                    [id (number->string (random 100000 999999))]))
                                     tcs)])]
      [_ m]))
  (parameterize ([current-messages-postprocessors (cons post (current-messages-postprocessors))])
    (repl)))
