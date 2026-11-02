#lang racket/base
(require rkt-llm-cli/repl
         rkt-llm-cli/skills
         rkt-llm-cli/file-tools
         rkt-llm-cli/mcp-tools
         rkt-llm-cli/mcp-client)

(provide (all-defined-out))

(define (agent-tools #:file-tools [file #t]
                     #:shell [shell #t]
                     #:mcp [mcp #t]
                     #:skills [skills #t])
  (append
   (if file (file-tools) '())
   (if shell (list shell_exec) '())
   (if skills
       (with-handlers ([exn:fail? (λ (e) '())])
         (list (make-skill-tool (build-path (find-system-path 'home-dir) ".agent" "skills"))))
       '())
   (if mcp
       (with-handlers ([exn:fail? (λ (e) '())])
         (unless (current-mcp-client)
           (current-mcp-client (make-mcp-client (load-mcp-config (build-path (find-system-path 'home-dir) ".agent" "mcp.json")))))
         (mymcp-tools))
       '())))

(define (agent [tools (agent-tools)])
  (with-system-default-prefix (λ () (use-tools tools))))