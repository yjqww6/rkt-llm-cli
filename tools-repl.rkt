#lang typed/racket/base/shallow
(require "main.rkt"
         "tool-template.rkt"
         racket/match
         racket/list)
(provide current-tool-callback execute tool-repl-prompt make-auto-execute-chat
         with-nous-tools)

(define-type ToolCallback (-> HashTableTop (Option String)))
(define current-tool-callback
  (make-parameter (ann (λ (tcs) (error 'tool-callback)) ToolCallback)))
(define current-tool-parser
  (make-parameter (ann (λ (s) '()) (-> String (Listof ToolCall)))))

(define (tool-repl-prompt)
  (string-append "TOOL:" (default-repl-prompt)))

(define (execute) : Void
  (define (calling [tcs : (Listof ToolCall)])
    (define tool-args : (Listof HashTableTop)
      (map
       (λ ([tc : ToolCall])
         (match-define (ToolCall name args id) tc)
         (hasheq 'name name 'arguments args))
       tcs))
    (let/ec k : Void
      (define tool-resps : (Listof String)
        (for/list ([arg (in-list tool-args)])
          (when (terminal-port? (current-output-port))
            (printf "\033[34mExecuting ~a: ~a\033[0m~%" (hash-ref arg 'name) (hash-ref arg 'arguments)))
          (define r
            ((current-tool-callback) arg))
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
  (define tools (current-tools))
  (define (system-rewrite [s : (Option String)]) : (Option String)
    (make-nous-system-template tools s))
  (parameterize ([current-tool-callback (λ ([j : HashTableTop])
                                          (define r (old-callback j))
                                          (cond
                                            [(not r) #f]
                                            [else
                                             (make-nous-response r)]))]
                 [current-tool-parser parse-nous-toolcall]
                 [current-tools '()]
                 [current-messages-preprocessors (cons
                                                  (compose1
                                                   (λ ([s : History]) (map tool->user s))
                                                   (λ ([s : History]) (map-system system-rewrite s)))
                                                  (current-messages-preprocessors))])
    (repl)))