#lang typed/racket/base/shallow
(require "main.rkt"
         "suffix.rkt"
         racket/match
         racket/list)
(provide (all-defined-out))

(define-parameter current-system #f : (Option String))
(define-parameter current-history '() : History)
(define-parameter current-response-id #f : (Option String))

(define (make-history [history : History] [new-msgs : (Listof Msg)])
  (define h (if (null? new-msgs) history (append history new-msgs)))
  (define sys (current-system))
  (if sys
      (cons (make-system sys) h)
      h))

(define (make-interactive-chat [chatter : Chatter]) : InteractiveChatter
  (define (prefix-chatter [prefix : (Option String)]) : Chatter
    (if (not prefix)
        chatter
        (λ (h s o)
          (s prefix 'content)
          (define prefill (make-assistant prefix))
          (define resp (chatter (append h (list prefill)) s o))
          (merge-message prefill resp))))
  (λ ([msg : Interactive] streaming options)
    (define history (current-history))
    (cond
      [(Continue? msg)
       (match/values
        (split-at-right history 1)
        [(past (list (and assist (struct* Msg ([role "assistant"])))))
         (define resp (chatter (make-history history '()) streaming options))
         (define new-resp (merge-message assist resp))
         (current-history (append past (list new-resp)))
         new-resp])]
      [else
       (define pchatter (prefix-chatter (InteractiveCommon-prefix msg)))
       (cond
         [(Redo? msg)
          (match/values
           (split-at-right history 1)
           [(history (list (struct* Msg ([role "assistant"]))))
            (define resp (pchatter (make-history history '()) streaming options))
            (current-history (append history (list resp)))
            resp])]
         [(ToolResult? msg)
          (define resp (pchatter (make-history history (ToolResult-result msg))
                                 streaming options))
          (current-history (append history (ToolResult-result msg) (list resp)))
          resp]
         [(User? msg)
          (define resp (pchatter (make-history history (list (User-msg msg)))
                                 streaming options))
          (current-history (append history (list (User-msg msg) resp)))
          resp])])))

(define (make-chat-by-template [completer : Completer] [chat-template : ChatTemplate]) : Chatter
  (λ (history streaming options)
    (define output (completer (chat-template history options) streaming options))
    (make-assistant output)))

(define (undo)
  (current-history (drop-right (current-history) 2)))

(define (clear)
  (current-history '())
  (current-response-id #f))

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

(define ((default-streaming) [s : String] [type : StreamingType])
  (cond
    [(eq? type 'think) (void)]
    [else
     (display s)
     (flush-output)]))

(define (gray-cot-streaming)
  (define last-type : (Option StreamingType) #f)
  (λ ([s : String] [type : StreamingType])
    (when (and last-type (not (eq? type last-type)))
      (newline))
    (set! last-type type)
    (define (f)
      (display s)
      (flush-output))
    (cond
      [(eq? type 'think)
       (call/color 'gray f #:reset? #f #:newline? #f)]
      [else (f)])))

(define (make-check-streaming [check : (-> Char Boolean)]
                              [up : (-> String StreamingType Void)]
                              #:include? [include? #f]) : (-> String StreamingType Void)
  (define done : Boolean #f)
  (λ (s t)
    (cond
      [(not done)
       (let loop ([i : Nonnegative-Fixnum 0])
         (when (< i (string-length s))
           (cond
             [(check (string-ref s i))
              (up (substring s (if include? i (+ i 1))) t)
              (set! done #t)]
             [else (loop (add1 i))])))]
      [else (up s t)])))

(define-parameter current-cot-suffix "</think>" : String)

(define (skip-content-cot-streaming)
  (define checker (make-suffixes-checker (list (current-cot-suffix))))
  (make-check-streaming
   (λ ([c : Char]) (string? (checker c)))
   (make-check-streaming
    (λ ([c : Char]) (not (char-whitespace? c)))
    (default-streaming)
    #:include? #t)))

(define current-streaming (make-parameter default-streaming))