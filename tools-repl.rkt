#lang typed/racket/base/shallow
(require "main.rkt"
         "tool-template.rkt"
         "private/types.rkt"
         "private/main.rkt"
         racket/match
         racket/list)
(provide execute tools-repl-loop)

(define-type ToolCallback (-> String String (Option String)))
(define current-tool-callback
  (make-parameter (ann (λ (sym tcs) (error 'tool-callback)) ToolCallback)))
(define current-tool-parser
  (make-parameter (ann (λ (s) '()) (-> String (Listof ToolCall)))))

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
      ((current-chat) (ToolResult #f tool-msgs))))
  (match (current-history)
    [(list _ ... (struct* Msg ([role "assistant"] [tool-calls tcs])))
     #:when (not (null? tcs))
     (calling tcs)]
    [(list _ ... (struct* Msg ([role "assistant"]
                               [content (and (? string?) (app (current-tool-parser) tcs))])))
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

(define ((tool->user [maker : (-> String String)]) [msg : Msg])
  (match msg
    [(struct* Msg ([role "tool"] [content (? string? content)]))
     (struct-copy Msg msg [role (ann "user" Role)] [content (maker content)])]
    [else msg]))

(define (map-system [f : (-> (Option String) (Option String))] [h : History]) : History
  (: s (Option String))
  (define-values (s r)
    (match h
      [(cons (struct* Msg ([role "system"] [content content])) r)
       (assert (string? content))
       (values content r)]
      [_ (values #f h)]))
  (cond
    [(f s) => (λ (sys) (cons (make-system sys) r))]
    [else r]))

(define (with-nous-tools [repl : (-> Void)])
  (define old-callback (current-tool-callback))
  (define (system-rewrite [s : (Option String)]) : (Option String)
    (make-nous-system-template (map Tool-desc (current-tools)) s))
  (parameterize ([current-tool-parser parse-nous-toolcall]
                 [current-interactive-hooks (cons
                                             (ann
                                              (λ (i o)
                                                (values i (struct-copy Options o [tools '()])))
                                              InteractiveHook)
                                             (current-interactive-hooks))]
                 [current-messages-preprocessors (cons
                                                  (compose1
                                                   (λ ([s : History]) (map (tool->user make-nous-response) s))
                                                   (λ ([s : History]) (map-system system-rewrite s)))
                                                  (current-messages-preprocessors))])
    (repl)))

(define (query-arg-type [tool : Tool] [param : Symbol])
  (match (json-ref (Tool-desc tool) 'function 'parameters 'properties param 'type)
    [(or "string" "str" "text" "varchar" "char" "enum") 'str]
    [_ 'json]))

(define (with-step-tools [repl : (-> Void)])
  (define old-callback (current-tool-callback))
  (define (system-rewrite [s : (Option String)]) : (Option String)
    (make-step-system-template (map Tool-desc (current-tools)) s))
  (parameterize ([current-tool-parser (λ ([s : String])
                                        (parse-step-response
                                         s
                                         (λ ([name : String] [param : Symbol]) : (U 'str 'json)
                                           (cond
                                             [(memf (λ ([t : Tool]) (string=? name (Tool-name t))) (current-tools))
                                              =>
                                              (λ (tools)
                                                (query-arg-type (car tools) param))]
                                             [else (error 'default-tool-callback "~a not found" name)]))))]
                 [current-interactive-hooks (cons
                                             (ann
                                              (λ (i o)
                                                (values i (struct-copy Options o [tools '()])))
                                              InteractiveHook)
                                             (current-interactive-hooks))]
                 [current-messages-preprocessors (cons
                                                  (compose1
                                                   (λ ([s : History]) (map (tool->user make-step-response) s))
                                                   (λ ([s : History]) (map-system system-rewrite s)))
                                                  (current-messages-preprocessors))])
        (repl)))

(define (with-mistral-tools [repl : (-> Void)])
  (define (post [m : Msg])
    (match m
      [(struct* Msg ([role "assistant"] [content (and (? string?) (app parse-mistral-toolcall tcs))]))
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

(module control racket/base
  (require racket/control)
  (provide (all-defined-out))
  (define (call/reset proc)
    (reset (proc)))
  (define (call/shift proc)
    (shift k (proc k))))

(require/typed/provide
 'control
 [call/reset (-> (-> Void) Void)]
 [call/shift (-> (-> (-> Void) Void) (Values))])

(define (tools-repl-loop #:auto [auto? #t] #:manual [manual #f] [tools : (Listof Tool)])
  (parameterize ([current-tools tools]
                 [current-tool-callback default-tool-callback]
                 [current-repl-prompt (make-prefix-repl-prompt "TOOL")])
    (call/reset
     (λ ()
       (when manual
         (call/shift
          (λ (k) (cond [(eq? manual 'step3) (with-step-tools k)]
                       [(eq? manual 'mistral) (with-mistral-tools k)]
                       [else (with-nous-tools k)]))))
       (when auto?
         (call/shift
          (λ (k) (parameterize ([current-chat (make-auto-execute-chat (current-chat))])
                   (k)))))
       (repl-loop)
       (void)))))
