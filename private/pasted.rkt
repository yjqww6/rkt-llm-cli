#lang typed/racket/base/shallow
(require "main.rkt"
         racket/match)
(provide current-pasted
         clear-pasted
         remember-pasted
         restore-pasted
         current-paste
         do-paste)

(define-parameter current-pasted '() : (Listof (U String Image)))
(define (clear-pasted)
  (current-pasted '()))

(define pasted-history : (Thread-Cellof (Option (Weak-HashTable Msg (Listof (U String Image))))) (make-thread-cell #f #f))
(thread-cell-set! pasted-history (ann (make-weak-hasheq) (Weak-HashTable Msg (Listof (U String Image)))))

(define (find-new-user [h : History] [base : History]) : (Option Msg)
  (let loop ([h h] [b base])
    (match* (h b)
      [((cons _ h1) (cons _ b1)) (loop h1 b1)]
      [((cons (and u (struct* Msg ([role "user"]))) _) '()) u]
      [(_ _) #f])))

(define (remember-pasted [base : History] [h : History] [p : (Listof (U String Image))])
  (define w (thread-cell-ref pasted-history))
  (when w
    (define u (find-new-user h base))
    (when u
      (hash-set! w u p))))

(define (restore-pasted [base : History] [h : History]) : (Option (Listof (U String Image)))
  (define w (thread-cell-ref pasted-history))
  (and w
       (let ([u (find-new-user h base)])
         (and u
              (hash-ref w u (λ () #f))))))

(define-parameter current-paste (λ () #f) : (-> (Option (Listof (U Image String)))))

(define (do-paste [pasted : (U Image String (Listof (U Image String)))] [append? : Boolean #t])
  (define p
    (cond
      [(list? pasted) pasted]
      [else (list pasted)]))
  (cond
    [append? (current-pasted (append (current-pasted) p))]
    [else (current-pasted p)]))