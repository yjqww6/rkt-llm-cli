#lang typed/racket/base/shallow
(require "main.rkt"
         racket/match
         racket/list)
(provide (all-defined-out))

(define current-system (make-parameter (ann #f (Option String))))
(define current-history (make-parameter (ann '() History)))

(define (make-history [history : History] [new-msgs : (Listof Msg)])
  (define h (append history new-msgs))
  (define sys (current-system))
  (if sys
      (cons (make-system sys) h)
      h))

(define (make-interactive-chat [chatter : Chatter]) : InteractiveChatter
  (λ ([msg : Interactive] streaming options)
    (define history (current-history))
    (cond
      [(null? msg) (error 'chat "null msg: ~a" msg)]
      [(eq? msg 'redo)
       (match/values
        (split-at-right history 2)
        [(history (list user assist))
         (define resp (chatter (make-history history (list user)) streaming options))
         (current-history (append history (list user resp)))
         resp])]
      [(eq? msg 'continue)
       (match/values
        (split-at-right history 1)
        [(past (list (and assist (struct* Msg ([role "assistant"])))))
         (define resp (chatter (make-history history '()) streaming options))
         (define new-resp (merge-message assist resp))
         (current-history (append past (list new-resp)))
         new-resp])]
      [(or (string? msg) (Msg? msg))
       (define user (if (string? msg) (make-user msg) msg))
       (define resp (chatter (make-history history (list user))
                             streaming options))
       (current-history (append history (list user resp)))
       resp]
      [(and (pair? msg) (string? (cdr msg)))
       (define prefill (make-assistant (cdr msg)))
       (streaming (cdr msg))
       (define resp (chatter (make-history history (list (car msg) prefill)) streaming options))
       (define new-resp (merge-message prefill resp))
       (current-history (append history (list (car msg) new-resp)))
       new-resp]
      [else
       (define resp (chatter (make-history history msg)
                             streaming options))
       (current-history (append history msg (list resp)))
       resp])))

(define (make-chat-by-template [completer : Completer] [chat-template : ChatTemplate]) : Chatter
  (λ (history streaming options)
    (define output (completer (chat-template history) streaming options))
    (make-assistant output)))

(define (undo-history [history : History]) : History
  (let loop ([history (reverse history)])
    (match history
      [(cons (struct* Msg ([role "user"])) rest) (reverse rest)]
      [(cons _ rest) (loop rest)])))

(define (undo)
  (current-history (undo-history (current-history))))

(define (clear)
  (current-history '()))