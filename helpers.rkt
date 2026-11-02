#lang typed/racket/base/shallow
(require "private/main.rkt"
         "private/chat.rkt"
         "private/pasted.rkt"
         "private/endpoint.rkt"
         racket/match
         racket/string
         racket/list
         typed/racket/date)
(provide (all-defined-out))

(define (trace-network! [only : (U 'send 'recv #f) #f] [output : Output-Port (current-output-port)])
  (current-network-trace
   (lambda (type data)
     (when (or (not only) (eq? type only))
       (fprintf output "~a: ~a~%" type data)))))

(define (untrace-network!)
  (current-network-trace void))

(define (use-authorization-token [token : (Option String) #f])
  (define tok
    (or token (getenv "LLM_API_TOKEN")))
  (assert tok)
  (current-headers
   (cons (string-append "Authorization: Bearer " tok)
         (current-headers))))

(define (with-system-prefix [system-prefixes : (Listof (-> String))] [thunk : (-> Any)])
  (define (system-rewrite [h : History])
    (define prefixes (for/list : (Listof String) ([f (in-list system-prefixes)]) (f)))
    (match h
      [(cons (struct* Msg ([role "system"] [content content])) r)
       (assert (string? content))
       (cons (make-system (string-join (append prefixes (list content)) "\n")) r)]
      [_ (cons (make-system (string-join prefixes "\n")) h)]))
  (parameterize ([current-messages-preprocessors (cons system-rewrite (current-messages-preprocessors))])
    (thunk)))

(define (make-system-date-prefix)
  (format "Current Date: ~a" (date->string (current-date))))

(define (make-system-dir-prefix)
  (format "Current Directory: ~a" (path->string (current-directory))))

(define-parameter current-system-date-prefix "" : String)
(define-parameter current-system-dir-prefix "" : String)

(define (with-system-default-prefix [thunk : (-> Any)])
  (parameterize ([current-system-dir-prefix (make-system-dir-prefix)]
                 [current-system-date-prefix (make-system-date-prefix)])
    (with-system-prefix (list current-system-dir-prefix current-system-date-prefix) thunk)))

(define (refresh-system-prefix)
  (current-system-date-prefix (make-system-date-prefix))
  (current-system-dir-prefix (make-system-dir-prefix)))

(define (last-response)
  (Msg-content (last (current-history))))

(define (undo/pasted)
  (define h (current-history))
  (undo)
  (define base (current-history))
  (define r (restore-pasted base h))
  (when r
    (current-pasted r)))
