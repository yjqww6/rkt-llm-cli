#lang typed/racket/base/shallow
(require "types.rkt"
         racket/match
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))
(provide (all-defined-out))

(define-type Role (U "system" "user" "assistant" "tool"))
(struct ToolCall ([name : String] [arguments : String] [id : String]) #:prefab)
(struct Msg ([role : Role] [content : String] [images : (Listof Bytes)] [tool-calls : (Listof ToolCall)] [tool-call-id : (Option String)]) #:prefab)

(define (merge-message [a : Msg] [b : Msg])
  (match* (a b)
    [((Msg a b c d x) (Msg e f g h y))
     (Msg a (string-append b f) (append c g) (append d h) (or x y))]))

(define (make-user [prompt : String])
  (Msg "user" prompt '() '() #f))

(define (make-system [prompt : String])
  (Msg "system" prompt '() '() #f))

(define (make-assistant [content : String])
  (Msg "assistant" content '() '() #f))

(define (make-tool [resp : String] [id : (Option String)])
  (Msg "tool" resp '() '() id))

(define (make-msg [role : Role] [content : String])
  (Msg role content '() '() #f))

(define-type History (Listof Msg))

(define-type Chatter (History (-> String Void) Options -> Msg))
(define-type Interactive (U String Msg (Pairof Msg String) (Pairof 'result (Listof Msg)) 'redo 'continue))
(define-type InteractiveChatter (Interactive (-> String Void) Options -> Msg))
(define-type Completer (String (-> String Void) Options -> String))
(define-type ChatTemplate (History -> String))
(define-type InteractiveHook (-> Interactive Options (Values Interactive Options)))

(: hash-and (All (K V) (->* ((Immutable-HashTable K V)) #:rest-star (K (Option V)) (Immutable-HashTable K V))))
(define (hash-and h . kv*)
  (let loop ([h : (Immutable-HashTable K V) h]
             [kv* : (Rec x (U Null (List* K (Option V) x))) kv*])
    (match kv*
      ['() h]
      [(list* k v r)
       (if v
           (loop (hash-set h k v) r)
           (loop h r))])))

(: hash-build (All (K V) (->* () #:rest-star (K (Option V)) (Immutable-HashTable K V))))
(define (hash-build . kv*)
  (apply hash-and ((inst hasheq K V)) kv*))

(: null->false (All (a) (Listof a) -> (Option (Pairof a (Listof a)))))
(define (null->false l)
  (if (null? l)
      #f
      l))

(: json-ref (JSExpr (U Integer Symbol) * -> JSExpr))
(define (json-ref j . k*)
  (let loop ([j j] [k* k*])
    (match* (j k*)
      [(_ '()) j]
      [((? list? j) (cons (? integer? k) k*))
       (loop (list-ref j k) k*)]
      [((? hash? j) (cons (? symbol? k) k*))
       (loop (hash-ref j k) k*)]
      [(_ _) #f])))

(: merge-right (All (a) (Option a) (Option a) -> (Option a)))
(define (merge-right a b)
  (or b a))

(: merge-right/null (All (a) (U Null a) (U Null a) -> (U Null a)))
(define (merge-right/null a b)
  (if (not (null? b)) b a))

(define-syntax-parser define-option
  [(_ Name:id [Id:id (~literal :) Type Def:expr merger:id] ...)
   #:with make-Name (format-id #'Name "make-~a" #'Name)
   #:with current-Name (format-id #'Name "current-~a" #'Name)
   #:with merge-Name (format-id #'Name "merge-~a" #'Name)
   #:with (current-Id ...) (map (λ (id) (format-id id "current-~a" id)) (syntax->list #'(Id ...)))
   #:with (K ...) (datum->syntax #'Name
                                 (map string->keyword
                                      (map symbol->string
                                           (syntax->datum #'(Id ...)))))
   #:with (a ...) (generate-temporaries #'(Id ...))
   #:with (b ...) (generate-temporaries #'(Id ...))
   #'(begin
       (struct Name ([Id : Type] ...) #:prefab)
       (define (make-Name (~@ K [Id : Type Def]) ...)
         (Name Id ...))
       (define current-Id (make-parameter (ann Def Type)))
       ...
       (define (current-Name) : Name
         (Name (current-Id) ...))
       (define (merge-Name [opta : Name] [optb : Name])
         (match-define (Name a ...) opta)
         (match-define (Name b ...) optb)
         (Name (merger a b) ...)))])

(define-option Options
  [endpoint : (Option String) #f merge-right]
  [headers : (Listof String) '() append]
  [stream : (U Null Boolean) '() merge-right/null]
  [tools : (Listof JSExpr) '() append]
  [max-tokens : (Option Integer) #f merge-right]
  [stop : (Listof String) '() append]
  [grammar : (Option String) #f merge-right]
  ;; sampling
  [temperature : (Option Flonum) #f merge-right]
  [top-k : (Option Positive-Integer) #f merge-right]
  [top-p : (Option Flonum) #f merge-right]
  [min-p : (Option Flonum) #f merge-right]
  [repeat-penalty : (Option Flonum) #f merge-right]
  ;; ollama
  [model : (Option String) #f merge-right]
  [context-window : (Option Exact-Positive-Integer) #f merge-right])

(define current-verbose (make-parameter (ann #f Boolean)))
(define current-network-trace (make-parameter (ann void (-> (U 'send 'recv) (U Bytes String) Void))))

(define break-prompt-tag : (Prompt-Tagof Void (-> (-> Nothing) Void))
  (make-continuation-prompt-tag))

(: call/interrupt (-> (-> Void) (-> Void) Void))

(define (call/interrupt proc on-abort)
  (with-handlers* ([(λ (e) (and (exn:break? e)
                                (continuation-prompt-available? break-prompt-tag)))
                    (λ (e)
                      (call/cc
                       (λ ([cc : (-> Nothing)])
                         (abort-current-continuation break-prompt-tag cc)))
                      (on-abort))])
    (proc)))

(: interactive-is-user? (-> Interactive Boolean : (U Msg String (Pairof Msg String))))
(define (interactive-is-user? s)
  (cond
    [(or (Msg? s) (string? s)) #t]
    [(and (pair? s) (Msg? (car s))) #t]
    [else #f]))

(: interactive-is-user&prefill? (-> Interactive Boolean : (Pairof Msg String)))
(define (interactive-is-user&prefill? s)
  (cond
    [(and (pair? s) (Msg? (car s))) #t]
    [else #f]))

(: interactive-is-tool? (-> Interactive Boolean : (Pairof 'result (Listof Msg))))
(define (interactive-is-tool? s)
  (cond
    [(and (pair? s) (eq? 'result (car s))) #t]
    [else #f]))

(: interactive-is-command? (-> Interactive Boolean : (U 'redo 'continue)))
(define (interactive-is-command? s)
  (cond
    [(or (eq? s 'redo) (eq? s 'continue)) #t]
    [else #f]))