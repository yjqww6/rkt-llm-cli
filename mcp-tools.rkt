#lang racket/base
(require "main.rkt"
         "tools.rkt"
         "mcp-client.rkt"
         json)
(provide mymcp-tools current-mcp-client)

(define current-mcp-client (make-parameter #f))

(define (mymcp-list-tools)
  (mcp-client-get-tools (current-mcp-client)))

(define (mymcp-tool-call name)
  (λ (arguments)
    (define j (mcp-client-call-tool (current-mcp-client) name (string->jsexpr arguments)))
    (hash-ref (list-ref (hash-ref j 'content) 0) 'text)))

(define (mymcp-tools)
  (define tools (mymcp-list-tools))
  (for/list ([t (in-list tools)])
    (Tool (hash-ref t 'name)
          (hasheq 'type "function" 'function t)
          (mymcp-tool-call (hash-ref t 'name)))))