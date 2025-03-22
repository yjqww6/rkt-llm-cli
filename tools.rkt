#lang racket/base
(require (for-syntax racket/base)
         "private/main.rkt"
         racket/match
         racket/string
         racket/system
         syntax/parse/define
         json)
(provide define-tool tool->string tools->string shell_exec (struct-out Tool))

(begin-for-syntax
  (define-syntax-class Param #:datum-literals (:)
    (pattern [Name:id : Type:id
                      (~alt
                       (~once (~seq #:desc Desc:string))
                       (~optional (~seq #:enum Enum:expr))
                       (~optional (~and (~seq #:def E:expr)
                                        (~bind [(Def 1) (list #'(λ () E))]
                                               [(Required 1) '()]))
                                  #:defaults ([(Def 1) '()]
                                              [(Required 1) (list #'Name)]))
                       (~optional (~seq #:items Items:expr)))
                      ...]
      #:with Prop
      #'(hasheq 'type (symbol->string 'Type)
                'description Desc
                (~? (~@ 'enum Enum))
                (~? (~@ 'items Items))))))

(define-syntax-parser define-tool
  [(_ (Name:id Param:Param ...) #:desc Desc:expr Body:expr ...+)
   #'(define Name
       (Tool
        (symbol->string 'Name)
        (hasheq
         'type "function"
         'function
         (hasheq 'name (symbol->string 'Name)
                 'description Desc
                 'parameters
                 (hasheq 'type "object"
                         'properties (hasheq (~@ 'Param.Name Param.Prop) ...)
                         'required (map symbol->string (list 'Param.Required ... ...)))))
        (λ (arg)
          (define h (string->jsexpr arg))
          (define Param.Name (hash-ref h 'Param.Name Param.Def ...)) ...
          Body ...)))])

(define (tool->string tool)
  (jsexpr->string (Tool-desc tool)))

(define (tools->string tools)
  (jsexpr->string (map Tool-desc tools)))

(define (system/string cmd)
  (define o (open-output-string))
  (parameterize ([current-output-port o]
                 [current-error-port o])
    (system cmd)
    (get-output-string o)))

(define-tool (shell_exec [cmd : string #:desc "command line to be executed"])
  #:desc (format "execute shell command. system type: ~a" (system-type 'os))
  (call/color
   'red
   (λ () (printf "~a\tComfirm[y/n]:" cmd))
   #:newline? #f)
  (if (string=? "y" (read-line))
      (system/string cmd)
      #f))
