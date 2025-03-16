#lang typed/racket/base/shallow
(require racket/match
         racket/fixnum)

(provide make-suffixes-checker)

(struct Check
  ([pattern : String]
   [pi : (Vectorof Nonnegative-Fixnum)]
   [current-state : Nonnegative-Fixnum])
  #:mutable)

(define (compute-kmp-restart-vector [pattern : String]) : (Vectorof Nonnegative-Fixnum)
  (define m (string-length pattern))
  (define pi  : (Vectorof Nonnegative-Fixnum) (make-vector m 0))
  (let loop ([i : Positive-Fixnum 1] [length : Nonnegative-Fixnum 0])
    (cond
      [(fx>= i m) pi]
      [(char=? (string-ref pattern i)
               (string-ref pattern length))
       (vector-set! pi i (fx+ 1 length))
       (loop (fx+ 1 i) (fx+ 1 length))]
      [(fx= 0 length)
       (loop (fx+ 1 i) 0)]
      [else
       (loop i (vector-ref pi (sub1 length)))])))

(define (make-suffix-check [pattern : String])
  (Check pattern (compute-kmp-restart-vector pattern) 0))

(define (check-suffix-step! [check : Check] [c : Char]) : (U Boolean String)
  (match-define (Check pattern pi state) check)
  (define n (string-length pattern))
  (define next-state : Nonnegative-Fixnum
    (let loop ([state : Nonnegative-Fixnum state])
      (cond
        [(and (fx< state n)
              (char=? (string-ref pattern state) c))
         (fx+ state 1)]
        [(fx= 0 state) state]
        [else
         (loop (vector-ref pi (- state 1)))])))
  (set-Check-current-state! check next-state)
  (cond
    [(= next-state n) pattern]
    [(> next-state 0) #t]
    [else #f]))

(define (make-suffixes-checker [stops : (Listof String)])
  (define checkers (map make-suffix-check stops))
  (λ ([b : Char]) : (U Boolean String)
    (define ls : (Listof (U Boolean String))
      (for/list ([c (in-list checkers)])
        (check-suffix-step! c b)))
    (cond
      [(memf string? ls) => car]
      [else (and (ormap (inst values Any) ls) #t)])))