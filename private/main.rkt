#lang typed/racket/base/shallow
(require "types.rkt"
         racket/match
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))
(provide (all-defined-out))

(define-type Role (U "system" "user" "assistant" "tool"))
(struct ToolCall ([name : String] [arguments : String] [id : String]) #:prefab)
(struct Image ([data : Bytes]) #:prefab)
(struct Msg ([role : Role]
             [content : (U String (Listof (U String Image)))]
             [tool-calls : (Listof ToolCall)] [tool-call-id : (Option String)]) #:prefab)

(define (merge-message [a : Msg] [b : Msg])
  (match* (a b)
    [((Msg a b c d) (Msg e f g h))
     (Msg a
          (if (string? b)
              (if (string? f) (string-append b f) (cons b f))
              (if (string? f) (append b (list f)) (append b f)))
          (append c g)
          (or d h))]))

(define (make-user [prompt : String])
  (Msg "user" prompt '() #f))

(define (make-system [prompt : String])
  (Msg "system" prompt '() #f))

(define (make-assistant [content : String])
  (Msg "assistant" content '() #f))

(define (make-tool [resp : String] [id : (Option String)])
  (Msg "tool" resp '() id))

(define (make-msg [role : Role] [content : String])
  (Msg role content '() #f))

(define-type History (Listof Msg))

(struct Tool ([name : String] [desc : JSExpr] [callback : (-> String (Option String))]))

(struct InteractiveCommon ([prefix : (Option String)]))
(struct User InteractiveCommon ([msg : Msg]))
(struct ToolResult InteractiveCommon ([result : (Listof Msg)]))
(struct Redo InteractiveCommon ())
(struct Continue ())

(define-type Chatter (History (-> String Void) Options -> Msg))
(define-type Interactive (U User ToolResult Redo Continue))
(define-type InteractiveChatter (Interactive (-> String Void) Options -> Msg))
(define-type Completer (String (-> String Void) Options -> String))
(define-type ChatTemplate (History Options -> String))
(define-type InteractiveHook (-> Interactive Options (Values Interactive Options)))

(define-type (Nullable t) (U 'null t))

(: is-null? (All (a) (-> (Nullable a) Boolean : 'null)))
(define (is-null? a) (eq? 'null a))

(: hash-add (All (K V) (->* ((Immutable-HashTable K V)) #:rest-star (K (Nullable V)) (Immutable-HashTable K V))))
(define (hash-add h . kv*)
  (let loop ([h : (Immutable-HashTable K V) h]
             [kv* : (Rec x (U Null (List* K (U 'null V) x))) kv*])
    (match kv*
      ['() h]
      [(list* k v r)
       (if (is-null? v)
           (loop h r)
           (loop (hash-set h k v) r))])))

(: hash-build (All (K V) (->* () #:rest-star (K (Nullable V)) (Immutable-HashTable K V))))
(define (hash-build . kv*)
  (apply hash-add ((inst hasheq K V)) kv*))

(: null->nullable (All (a) (Listof a) -> (Nullable (Pairof a (Listof a)))))
(define (null->nullable l)
  (if (null? l)
      'null
      l))

(: false->nullable (All (a) (Option a) -> (Nullable a)))
(define (false->nullable a)
  (if a a 'null))

(: json-ref (JSExpr (U Integer Symbol) * -> JSExpr))
(define (json-ref j . k*)
  (let loop ([j j] [k* k*])
    (match* (j k*)
      [(_ '()) j]
      [((? list? j) (cons (? integer? k) k*))
       (if (> (length j) k)
           (loop (list-ref j k) k*)
           'null)]
      [((? hash? j) (cons (? symbol? k) k*))
       (if (hash-has-key? j k)
           (loop (hash-ref j k) k*)
           'null)]
      [(_ _) 'null])))

(: merge-right (All (a) (Nullable a) (Nullable a) -> (Nullable a)))
(define (merge-right a b)
  (if (is-null? b) a b))

;; enforcing early shape check
(define-syntax-parse-rule (define-parameter Name:id Def:expr (~literal :) Type)
  (define Name (make-parameter (ann Def Type) (λ ([v : Type]) v))))

(begin-for-syntax
  (define-syntax-class Opt-Type #:datum-literals (:)
    (pattern ((~literal Nullable) _)
      #:with Merger #'merge-right
      #:with Def #''null)
    (pattern ((~literal Listof) _)
      #:with Merger #'append
      #:with Def #''())))

(define-syntax-parser define-option
  [(_ Name:id [Id:id (~literal :) Type:Opt-Type] ...)
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
       (define (make-Name (~@ K [Id : Type Type.Def]) ...)
         (Name Id ...))
       (define-parameter current-Id Type.Def : Type)
       ...
       (define (current-Name) : Name
         (Name (current-Id) ...))
       (define (merge-Name [opta : Name] [optb : Name])
         (match-define (Name a ...) opta)
         (match-define (Name b ...) optb)
         (Name (Type.Merger a b) ...)))])

(define-option Options
  [endpoint : (Nullable String)]
  [headers : (Listof String)]
  [stream : (Nullable Boolean)]
  [tools : (Listof Tool)]
  [max-tokens : (Nullable Integer)]
  [stop : (Listof String)]
  [grammar : (Nullable String)]
  ;; sampling
  [temperature : (Nullable Flonum)]
  [top-k : (Nullable Positive-Integer)]
  [top-p : (Nullable Flonum)]
  [min-p : (Nullable Flonum)]
  [repeat-penalty : (Nullable Flonum)]
  ;; ollama
  [model : (Nullable String)]
  [context-window : (Nullable Exact-Positive-Integer)])

(define-parameter current-verbose #f : Boolean)
(define-parameter current-network-trace void : (-> (U 'send 'recv) (U Bytes String) Void))

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

(define (call/color [color : (U 'blue 'red)] [thunk : (-> Void)]
                    #:output [output : Output-Port (current-output-port)]
                    #:reset? [reset? : Boolean #t]
                    #:newline? [newline? : Boolean #t])
  (when reset?
    (define-values (line col pos) (port-next-location output))
    (when (and col (> col 0))
      (newline output)))
  (cond
    [(terminal-port? output)
     (write-string (cond
                     [(eq? color 'red) "\033[31m"]
                     [(eq? color 'blue) "\033[34m"])
                   output)
     (thunk)
     (write-string "\033[0m" output)]
    [else (thunk)])
  (when newline?
    (newline output)))