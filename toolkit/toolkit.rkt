#lang racket/base
(require json
         racket/port
         net/url
         rkt-llm-cli/tools)
(provide (all-defined-out))

(define (mymcp-list-tools)
  (define p (get-pure-port (string->url "http://localhost:9876/tools")))
  (define j (read-json p))
  (close-input-port p)
  (hash-ref j 'tools))

(define (mymcp-tool-call name)
  (λ (arguments)
    (define r
      (post-pure-port
       (string->url "http://localhost:9876/tool_call")
       (jsexpr->bytes (hasheq 'name name 'arguments (string->jsexpr arguments)))))
    (define js (port->string r #:close? #t))
    (define j (string->jsexpr js))
    (hash-ref (list-ref (hash-ref j 'content) 0) 'text)))

(define (mymcp-tools)
  (define tools (mymcp-list-tools))
  (for/list ([t (in-list tools)])
    (Tool (hash-ref t 'name)
          (hasheq 'type "function" 'function t)
          (mymcp-tool-call (hash-ref t 'name)))))