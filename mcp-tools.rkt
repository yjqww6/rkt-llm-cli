#lang racket/base
(require "tools.rkt"
         "mcp-client.rkt"
         racket/match
         json)
(provide mymcp-tools current-mcp-client load-mcp-config)

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

(define (load-mcp-config path)
  (call-with-input-file path
    (λ (p)
      (define j (read-json p))
      (for/fold ([cfgs '()])
                ([(k v) (in-hash (hash-ref j 'mcpServers))])
        (match v
          [(hash* ['url url])
           (cons (mcp-http-server-config url) cfgs)]
          [(hash* ['command command] ['args args #:default '()])
           (cons (mcp-stdio-server-config command args) cfgs)]
          [_ cfgs])))))