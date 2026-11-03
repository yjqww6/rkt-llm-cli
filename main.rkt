#lang typed/racket/base/shallow
(require "private/main.rkt"
         "private/chat.rkt"
         "private/endpoint.rkt"
         "private/pasted.rkt"
         "helpers.rkt"
         racket/match
         racket/list
         racket/port
         racket/bool
         racket/file
         racket/runtime-path)
(provide (all-defined-out)
         (all-from-out "private/main.rkt")
         (all-from-out "private/chat.rkt")
         (all-from-out "private/endpoint.rkt")
         (all-from-out "private/pasted.rkt")
         (all-from-out "helpers.rkt"))

(define #:forall (a) (call-with-cust (thunk : (-> a)))
  (define cust (make-custodian))
  (dynamic-wind void
                (λ ()
                  (parameterize ([current-custodian cust])
                    (thunk)))
                (λ () (custodian-shutdown-all cust))))

(define (chat [i : Interactive])
  (call-with-cust
   (λ ()
     (define s ((current-streaming)))
     ((current-interactive-chatter) i s (current-Options))
     (s "\n" 'content))))

(define (call/break [thunk : (-> Void)])
  (call-with-continuation-prompt
   thunk
   break-prompt-tag
   (λ ([cc : (-> Nothing)])
     (newline)
     (display "Accept as history[y/n]:")
     (define line (read-line (current-input-port) 'any))
     (when (and (not (eof-object? line)) (regexp-match? #px"^\\s*y" line))
       (cc)))))

(define-parameter current-output-prefix #f : (Option String))

(define (default-run-message [prompt : (U String 'continue 'redo)])
  (cond
    [(eq? prompt 'redo)
     (chat (Redo (current-output-prefix)))
     (current-output-prefix #f)]
    [(eq? prompt 'continue)
     (chat (Continue))]
    [else
     (define pasted (current-pasted))
     (define prefix (current-output-prefix))
     (define user
       (make-user (append (if (null? pasted)
                              '()
                              (append '("```\n") pasted '("\n```\n")))
                          (if (string? prompt) (list prompt) prompt))))
     (define base (current-history))
     (chat (User prefix user))
     (remember-pasted base (current-history) pasted)
     (current-pasted '())
     (current-output-prefix #f)]))

(define (default-repl-prompt)
  (string-append
   (let ([c (count string? (current-pasted))])
     (if (> c 0) (format "text:~a " c) ""))
   (let ([c (count Image? (current-pasted))])
     (if (> c 0) (format "img:~a " c) ""))
   (if (current-output-prefix) "pre " "")
   ">>>"))
(define current-repl-prompt (make-parameter default-repl-prompt))

(define (make-prefix-repl-prompt [prefix : String] [on : (Option (Parameterof Boolean)) #f])
  (define old (current-repl-prompt))
  (λ () (if (implies on (on)) (string-append prefix " " (old)) (old))))

(define (default-repl-read)
  (newline)
  (display ((current-repl-prompt)))
  (read-line))

(define (default-repl-run [thunk : (-> Any)])
  (call-with-continuation-prompt (λ () (call/break (λ () (thunk) (void))))))

(define (default-repl-loop) : Any
  (let loop ()
    (define r ((current-repl-read)))
    (cond
      [(eof-object? r) (void)]
      [(string? r)
       ((current-repl-run)
        (λ ()
          ((current-repl-run-message) r)))
       (loop)]
      [else
       ((current-repl-run) r)
       (loop)])))

(define current-repl-read (make-parameter (ann default-repl-read (-> (U String EOF (-> Void))))))
(define current-repl-run (make-parameter (ann default-repl-run (-> (-> Any) Any))))
(define current-repl-run-message (make-parameter (ann default-run-message (-> (U String 'continue 'redo) Any))))
(define current-repl-loop (make-parameter (ann default-repl-loop (-> Any))))

(define (repl-loop) ((current-repl-loop)))

(define (warmup)
  (parameterize ([current-output-prefix (current-output-prefix)]
                 [current-pasted (current-pasted)]
                 [current-max-tokens 1]
                 [current-stream #f]
                 [current-history (current-history)]
                 [current-output-port (open-output-nowhere)])
    ((current-repl-run-message) "")))

(define (redo)
  ((current-repl-run-message) 'redo))

(define (continue)
  ((current-repl-run-message) 'continue))

(define-runtime-path COMPACT "COMPACT.txt")

(define compacted-history : (Weak-HashTable Msg History) (make-weak-hasheq))

(define (lookup-compacted [history : History (current-history)] #:recur? [recur : Boolean #t])
  (let loop ([h history] [compacted : (Listof History) '()])
    (match h
      [(cons (and user (struct* Msg ([role "user"]))) _)
       (cond
         [(hash-ref compacted-history user (λ () #f))
          =>
          (λ (h) (if recur (loop h (cons h compacted)) h))]
         [else compacted])]
      [else compacted])))

(define (compact-context)
  (call/color 'red (λ () (displayln "COMPACTING...")))
  (chat (User #f (make-user (file->string COMPACT))))
  (match-define (struct* Msg ([role "assistant"] [content (? string? compacted)])) (last (current-history)))
  (define new-history (make-user (list "```\n" compacted "\n...\nContinue")))
  (hash-set! compacted-history new-history (current-history))
  (clear)
  (current-history (list new-history)))
#;
(module+ main
  (use-endpoint)
  (repl-loop))
