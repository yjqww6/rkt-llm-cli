#lang racket/base
(require racket/class
         racket/file
         racket/lazy-require
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         racket/control
         "main.rkt"
         "tools.rkt"
         "tools-repl.rkt")
(provide (all-defined-out) (all-from-out "main.rkt" "tools.rkt" "tools-repl.rkt"))
(define-namespace-anchor here)

(lazy-require [racket/gui/base (get-file)])
(define (upload-image)
  (define p (get-file))
  (when p
    (current-paste-image (file->bytes p))))

(define (bitmap->bytes bm)
  (define b (open-output-bytes))
  (send bm save-file b 'jpeg)
  (get-output-bytes b))

(define clip
  (let ([c #f])
    (define (init)
      (unless c
        (set! c (dynamic-require 'racket/gui/base 'the-clipboard)))
      (current-milliseconds))
    (case-lambda
      [()
       (define t (init))
       (or (let ([b (send c get-clipboard-bitmap t)])
             (and b (bitmap->bytes b)))
           (send c get-clipboard-string t))]
      [(s)
       (define t (init))
       (send c set-clipboard-string s t)])))

(define (paste [append? #f])
  (define pasted (clip))
  (cond
    [(string? pasted)
     (cond
       [(not append?)
        (current-paste-text pasted)]
       [(current-paste-text)
        =>
        (λ (s) (current-paste-text (string-append s "\n" pasted)))]
       [else (current-paste-text pasted)])]
    [pasted (current-paste-image pasted)]
    [else (void)]))

(define (warmup)
  (parameterize ([current-output-prefix (current-output-prefix)]
                 [current-paste-text (current-paste-text)]
                 [current-paste-image (current-paste-image)]
                 [current-max-tokens 0]
                 [current-history (current-history)]
                 [current-output-port (open-output-nowhere)])
    (repl-chat "")))

(define (redo)
  (repl-chat 'redo))

(define (last-response)
  (match-define (hash 'content content #:open) (last (current-history)))
  content)

(define (use-tools #:auto [auto? #t] #:manual [manual #f] . tools)
  (parameterize ([current-tools (map tool-desc tools)]
                 [current-tool-callback (tools-callback tools)]
                 [current-repl-prompt tool-repl-prompt])
    (reset
     (when manual
       (shift k (cond [else (with-nous-tools k)])))
     (when auto?
       (shift k (parameterize ([current-chat (make-auto-execute-chat (current-chat))])
                  (k))))
     (repl-loop))))

(module+ main
  (require expeditor (submod expeditor configure)
           racket/port racket/cmdline racket/runtime-path
           (for-syntax racket/base))
  (define-runtime-module-path-index repl '(submod ".."))

  (define ns (namespace-anchor->empty-namespace here))
  (namespace-require 'racket ns)
  (namespace-require repl ns)

  (port-count-lines! (current-output-port))

  (define current-host (make-parameter "localhost"))
  (define current-port (make-parameter #f))
  (define current-tpl (make-parameter #f))
  (define current-use-ollama (make-parameter #f))
  (command-line
   #:program "rkt-llm-cli"
   #:once-each
   [("-v" "--verbose") "verbose messages" (current-verbose #t)]
   [("--host") h "host" (current-host h)]
   [("--port") p "port" (current-port (string->number p))]
   [("--tpl") p "chat template" (current-tpl p)]
   [("--ollama") "use ollama" (current-use-ollama #t)]
   [("--model") m "default model" (current-model m)]
   #:multi
   [("-r" "--require") file "required file" (namespace-require file ns)]
   [("-e" "--expr") expr "expression" (eval (read (open-input-string expr)) ns)])

  (current-chat
   (cond
     [(current-tpl)
      (cond
        [(current-use-ollama)
         (ollama-completion
          (current-tpl)
          (make-default-options (current-host) (or (current-port) 11434) "api/generate"))]
        [else
         (oai-compat-completion
          (current-tpl)
          (make-default-options (current-host) (or (current-port) 8080) "v1/completions"))])]
     [(current-use-ollama)
      (ollama-chat
       (make-default-options (current-host) (or (current-port) 11434) "api/chat"))]
     [else
      (oai-compat-chat
       (make-default-options (current-host) (or (current-port) 8080) "v1/chat/completions"))]))
  (current-complete
   (cond
     [(current-use-ollama)
      (ollama-complete
       (make-default-options (current-host) (or (current-port) 11434) "api/generate"))]
     [else
      (oai-compat-complete
       (make-default-options (current-host) (or (current-port) 8080) "v1/completions"))]))
  
  (define (command-input? in)
    (regexp-match-peek #px"^\\s*," in))

  ;; expeditor seems buggy, use M^v when paste massive texts
  (define (multi-input? in)
    (regexp-match-peek #px"^\"\"\"" in))
  (define multi-input-end #px"(\r\n|\n|\r)\"\"\"$")

  (define (multi-line-paste? in)
    (regexp-match-peek #px"^```(\r\n|\n|\r|$)" in))
  (define multi-input-paste-end #px"(\r\n|\n|\r)```$")

  (define (forward p in)
    (read-bytes (bytes-length (car p)) in))
  (struct message (content))
  (struct cmd (content))

  (define uploaded! #f)
  (struct refreshing ())

  (define running? (make-parameter #f))
  
  (define (run thunk)
    (parameterize-break
     #f
     (call-with-continuation-prompt
      (λ ()
        (parameterize ([running? #t])
          (parameterize-break
           #t
           (call-with-continuation-prompt
            thunk
            break-prompt-tag
            (λ (cc)
              (newline)
              (display "Accept as history[y/n]:")
              (when (regexp-match? #px"^\\s*y" (read-line (current-input-port) 'any))
                (cc))))))))))
  (define (run-chat s)
    (run (λ () (repl-chat s))))

  ;; prefixes
  ;; , starts a racket command
  ;; """ starts multi line block in expeditor without triggering shortcuts
  ;;; ^ starts a output prefix
  (define ((reader orig) in)
    (cond
      [uploaded!
       (set! uploaded! #f)
       (refreshing)]
      [(eof-object? (peek-byte in)) eof]
      [(eqv? #\^ (peek-char in))
       (read-char in)
       (current-output-prefix (port->string in))
       (refreshing)]
      [(eqv? #\$ (peek-char in))
       (read-char in)
       (current-system (port->string in))
       (refreshing)]
      [(eqv? #\! (peek-char in))
       (read-char in)
       (cmd (port->string in))]
      [(eqv? #\/ (peek-char in))
       (read-char in)
       (port->list read in)]
      [(command-input? in)
       =>
       (λ (p)
         (forward p in)
         (orig in))]
      [(multi-input? in)
       =>
       (λ (p)
         (forward p in)
         (message (car (regexp-split multi-input-end (port->string in)))))]
      [(multi-line-paste? in)
       =>
       (λ (p)
         (forward p in)
         (current-paste-text (car (regexp-split multi-input-paste-end (port->string in))))
         (refreshing))]
      [else (message (port->string in))]))

  (define ((ready orig) in)
    (cond
      [(command-input? in)
       =>
       (λ (p)
         (forward p in)
         (orig in))]
      [(multi-input? in)
       =>
       (λ (p)
         (forward p in)
         (regexp-match? multi-input-end in))]
      [(multi-line-paste? in)
       =>
       (λ (p)
         (forward p in)
         (regexp-match? multi-input-paste-end in))]
      [else #t]))

  (when (terminal-port? (current-input-port))
    (expeditor-configure)
    (define ee (expeditor-open '()))
    (current-expeditor-color-enabled #f)
    (expeditor-bind-key!
     "^C"
     (λ (ee entry c)
       (cond
         [(running?)
          (ee-reset-entry ee entry c)
          (break-thread (current-thread))]
         [else
          (ee-reset-entry ee entry c)])))
    (expeditor-bind-key!
     "\\ei"
     (λ (ee entry c)
       (upload-image)
       (set! uploaded! #t)
       #f))
    (expeditor-bind-key!
     "\\ev"
     (λ (ee entry c)
       (paste)
       (set! uploaded! #t)
       #f))
    
    (parameterize ([current-namespace ns]
                   [current-expeditor-reader
                    (reader (current-expeditor-reader))]
                   [current-expeditor-ready-checker
                    (ready (current-expeditor-ready-checker))])
      (define (loop)
        (define v (parameterize ([running? #f])
                    (expeditor-read ee #:prompt ((current-repl-prompt)))))
        (cond
          [(eof-object? v) (void)]
          [(refreshing? v) (loop)]
          [(message? v)
           (run-chat (message-content v))
           (loop)]
          [(cmd? v)
           (run (λ () (system (cmd-content v))))
           (loop)]
          [else
           (call-with-continuation-prompt
            (λ ()
              (call-with-values
               (λ () (run (λ () (eval v ns))))
               (λ r
                 (for ([v (in-list r)]
                       #:unless (void? v))
                   (println v))))))
           (loop)]))
      (current-repl-loop loop)
      (loop))
    (void (expeditor-close ee))))
