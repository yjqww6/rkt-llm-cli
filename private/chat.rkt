#lang typed/racket/base/shallow
(require "main.rkt"
         "suffix.rkt"
         racket/match
         racket/list)
(provide (all-defined-out))

(define-parameter current-system #f : (Option String))
(define-parameter current-history '() : History)

(define (make-history [history : History] [new-msgs : (Listof Msg)])
  (define h (if (null? new-msgs) history (append history new-msgs)))
  (define sys (current-system))
  (if sys
      (cons (make-system sys) h)
      h))

(define (make-interactive-chat [chatter : Chatter]) : InteractiveChatter
  (λ ([msg : Interactive] streaming options)
    (define history (current-history))
    (cond
      [(eq? msg 'redo)
       (match/values
        (split-at-right history 1)
        [(history (list (struct* Msg ([role "assistant"]))))
         (define resp (chatter (make-history history '()) streaming options))
         (current-history (append history (list resp)))
         resp])]
      [(eq? msg 'continue)
       (match/values
        (split-at-right history 1)
        [(past (list (and assist (struct* Msg ([role "assistant"])))))
         (define resp (chatter (make-history history '()) streaming options))
         (define new-resp (merge-message assist resp))
         (current-history (append past (list new-resp)))
         new-resp])]
      [(interactive-is-user&prefill? msg)
       (define prefill (make-assistant (cdr msg)))
       (streaming (cdr msg))
       (define resp (chatter (make-history history (list (car msg) prefill)) streaming options))
       (define new-resp (merge-message prefill resp))
       (current-history (append history (list (car msg) new-resp)))
       new-resp]
      [(interactive-is-user? msg)
       (define user (if (string? msg) (make-user msg) msg))
       (define resp (chatter (make-history history (list user))
                             streaming options))
       (current-history (append history (list user resp)))
       resp]
      [else
       (define resp (chatter (make-history history (cdr msg))
                             streaming options))
       (current-history (append history (cdr msg) (list resp)))
       resp])))

(define (make-chat-by-template [completer : Completer] [chat-template : ChatTemplate]) : Chatter
  (λ (history streaming options)
    (define output (completer (chat-template history options) streaming options))
    (make-assistant output)))

(define (undo)
  (current-history (drop-right (current-history) 2)))

(define (clear)
  (current-history '()))

(define (map-chatter [chatter : Chatter] [proc : (-> History History)])
  : Chatter
  (λ (h s o)
    (chatter (proc h) s o)))

(define (save-history [file : Path-String])
  (call-with-output-file*
   file
   (λ ([p : Output-Port])
     (write (current-history) p))
   #:exists 'replace))

(define (load-history [file : Path-String])
  (call-with-input-file* file
    (λ ([p : Input-Port])
      (current-history (cast (read p) History)))))

(define ((default-streaming) [s : String])
  (display s)
  (flush-output))

(define (make-check-streaming [check : (-> Char Boolean)]
                              [up : (-> String Void)]
                              #:include? [include? #f]) : (-> String Void)
  (define done : Boolean #f)
  (λ (s)
    (cond
      [(not done)
       (let loop ([i : Nonnegative-Fixnum 0])
         (when (< i (string-length s))
           (cond
             [(check (string-ref s i))
              (up (substring s (if include? i (+ i 1))))
              (set! done #t)]
             [else (loop (add1 i))])))]
      [else (up s)])))

(define-parameter current-cot-suffix "</think>" : String)

(define (hide-cot-streaming)
  (define checker (make-suffixes-checker (list (current-cot-suffix))))
  (make-check-streaming
   (λ ([c : Char]) (string? (checker c)))
   (make-check-streaming
    (λ ([c : Char]) (not (char-whitespace? c)))
    (default-streaming)
    #:include? #t)))

(define current-streaming (make-parameter default-streaming))