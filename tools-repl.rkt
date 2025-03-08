#lang typed/racket/base/shallow
(require "main.rkt"
         "tool-template.rkt"
         racket/match
         racket/list
         typed/json)
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
          (printf "Executing : ~a~%" arg)
          (define r
            ((current-tool-callback) arg))
          (unless r (k (void)))
          r))
      (define tool-msgs
        (map (λ ([rsp : String] [tc : ToolCall])
               (make-tool rsp (ToolCall-id tc)))
             tool-resps tcs))
      ((current-chat) tool-msgs)))
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

(define (make-tool->user-chat [old-chat : Chat (current-chat)])
  (λ ([s : Interactive]) : Void
    (old-chat
     (match s
       [(list msgs ...)
        #:when (andmap Msg? msgs)
        (map tool->user msgs)]
       [(? Msg? s) (tool->user s)]
       [else s]))))

(define (with-nous-tools [repl : (-> Void)])
  (define old-callback (current-tool-callback))
  (parameterize ([current-system (make-nous-system-template (current-tools) (current-system))]
                 [current-tool-callback (λ ([j : HashTableTop])
                                          (define r (old-callback j))
                                          (cond
                                            [(not r) #f]
                                            [else
                                             (make-nous-response r)]))]
                 [current-tool-parser parse-nous-toolcall]
                 [current-tools '()]
                 [current-chat (make-tool->user-chat)])
    (repl)))