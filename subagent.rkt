#lang racket/base
(require "main.rkt" "tools-loop.rkt" "tools.rkt" racket/list)
(provide subagent last-subagent maybe-continue-subagent current-subagent-hooks use-subagent-tools-hook)

(define ((use-subagent-tools-hook tools) next)
  (parameterize ([current-tools tools])
    (next)))

(define current-subagent-hooks (make-parameter '()))

(define (launch-subagent prompt inherit save-ctx-proc)
  (parameterize ([current-history (if inherit (current-history) '())])
    (let loop ([hooks (current-subagent-hooks)])
      (cond
        [(null? hooks)
         (call/cc
          (λ (k)
            (save-ctx-proc k)
            (when inherit
              (current-history (append (current-history) (list (make-tool "Subagent started. Context Switched." #f)))))
            (chat (User #f (make-user (list "You are the subagent, prompt:\n" prompt))))
            (execute-loop)
            (Msg-content (last (current-history)))))]
        [else ((car hooks) (λ () (loop (cdr hooks))))]))))

(define current-in-subagent (make-parameter #f))
(define last-subagent (make-parameter #f))

(define (maybe-continue-subagent [k (last-subagent)] #:before [before-thunk void] #:after [after-thunk void])
  (call-in-continuation
   k
   (λ ()
     (before-thunk)
     (when (null? (Msg-tool-calls (last (current-history))))
       (chat (Continue)))
     (execute-loop)
     (after-thunk)
     (Msg-content (last (current-history))))))

(define-tool (subagent [name : string #:desc "name of this subagent"]
                       [prompt : string #:desc "initial prompt to the subagent"]
                       [inherit : boolean #:desc "if true, the subagent could see parent conversation history"])
  #:desc "Fork a new subagent, return its final response. Subagents cannot be nested."
  (cond
    [(current-in-subagent) "Error: subagent couldn't be nested"]
    [else
     (parameterize ([current-in-subagent #t])
       (call/color
        'red
        (λ () (printf "Launching Subagent ~a" name)))
       (begin0
         (string-append
          (format "Response from ~a:\n" name)
          (launch-subagent prompt inherit last-subagent))
         (call/color
          'red
          (λ () (printf "Subagent Finishied ~a" name)))))]))