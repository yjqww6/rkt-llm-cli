#lang typed/racket/base/shallow
(require "main.rkt"
         racket/list
         racket/match
         racket/string)
(provide chat-template chatml skip-cot-tokens)

(define (split-prefill [messages : History])
  (match/values
   (split-at-right messages 1)
   [(h (list (struct* Msg ([role "assistant"] [content prefill]))))
    (values h prefill)]
   [(_ _)
    (values messages #f)]))

(define (split-system [messages : History])
  (match messages
    [(cons (struct* Msg ([role "system"] [content sys])) rest)
     (values sys rest)]
    [else (values #f messages)]))

(define (chatml [messages : History]) : String
  (define-values (his prefill) (split-prefill messages))
  (define s (open-output-string))
  (for ([msg (in-list his)])
    (match-define (struct* Msg ([role role] [content content])) msg)
    (write-string (format "<|im_start|>~a\n" role) s)
    (write-string content s)
    (write-string "<|im_end|>\n" s))
  (write-string "<|im_start|>assistant\n" s)
  (when prefill
    (write-string prefill s))
  (get-output-string s))

(define (gemma [messages : History]) : String
  (define-values (h prefill) (split-prefill messages))
  (define-values (sys his) (split-system h))
  (define s (open-output-string))
  (for ([msg (in-list his)]
        [i (in-naturals)])
    (match-define (struct* Msg ([role role] [content content])) msg)
    (define mapped-role (if (string=? role "assistant") "model" role))
    (write-string (format "<start_of_turn>~a\n" mapped-role) s)
    (when (and sys (= i 0))
      (write-string sys s)
      (write-string "\n\n" s))
    (write-string content s)
    (write-string "<end_of_turn>\n" s))
  (write-string "<start_of_turn>model\n" s)
  (when prefill
    (write-string prefill s))
  (get-output-string s))

(define (skip-cot-tokens [msgs : History] #:sep [sep : String "</think>"]) : History
  (define end (length msgs))
  (for/list ([m (in-list msgs)]
             [i (in-naturals 1)])
    (match m
      [(struct* Msg ([role "assistant"] [content content]))
       #:when (< i end)
       #:when (string-contains? content sep)
       (define new-content 
         (string-trim (last (string-split content sep)) "\n"
                      #:right? #f #:repeat? #t))
       (struct-copy Msg m [content new-content])]
      [else m])))

(define (chat-template [name : String] #:skip-cot? [skip-cot? : Boolean #t]) : ChatTemplate
  (define tpl
    (match name
      ["chatml" chatml]
      ["gemma" gemma]))
  (cond
    [(not skip-cot?) tpl]
    [else
     (compose1 tpl skip-cot-tokens)]))