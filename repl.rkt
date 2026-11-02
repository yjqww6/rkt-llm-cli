#lang racket/base
(require racket/class
         racket/file
         racket/lazy-require
         racket/list
         racket/port
         racket/system
         "main.rkt"
         "tools.rkt"
         "tools-loop.rkt")
(provide (all-defined-out) (all-from-out "main.rkt" "tools.rkt" "tools-loop.rkt"))
(define-namespace-anchor here)

(lazy-require [racket/gui/base (get-file)])
(define (upload-image)
  (define p (get-file))
  (when p
    (do-paste (list (Image (file->bytes p))) #t)))

(define (upload-text)
  (define p (get-file))
  (when p
    (do-paste (list (file->string p)) #t)))

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
             (and b (Image (bitmap->bytes b))))
           (send c get-clipboard-string t))]
      [(s)
       (define t (init))
       (send c set-clipboard-string s t)])))

(define (default-paste)
  (define x (clip))
  (cond
    [(equal? x "") #f]
    [else (list x)]))

(current-paste default-paste)

(define (paste [append? #t])
  (define pasted ((current-paste)))
  (when pasted
    (do-paste pasted append?)))

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
  (define current-path-prefix (make-parameter #f))
  (define current-use-response (make-parameter #f))
  (define current-native-paste (make-parameter #t))
  (define current-no-init (make-parameter #f))
  (command-line
   #:program "rkt-llm-cli"
   #:once-each
   [("-v" "--verbose") "verbose messages" (current-verbose #t)]
   [("--host") h "host" (current-host h)]
   [("--port") p "port" (current-port (string->number p))]
   [("--tpl") p "chat template" (current-tpl p)]
   [("--prefix") p "path prefix, default to v1/" (current-path-prefix p)]
   [("--response") "use openai response api" (current-use-response #t)]
   [("--model") m "default model" (current-model m)]
   [("--no-native-paste") "use os native paste" (current-native-paste #f)]
   [("--no-init") "not to load init file" (current-no-init #t)]
   #:multi
   [("-t" "--require") file "(require (file \"<file>\"))" (namespace-require (list 'file file) ns)]
   [("-l" "--lib") file "(require (lib \"<path>\"))" (namespace-require (list 'lib file) ns)]
   [("-e" "--expr") expr "expression" (eval (read (open-input-string expr)) ns)])

  (use-endpoint #:type (cond
                         [(current-use-response) 'oai-response]
                         [(current-tpl) 'oai-completion]
                         [else 'oai-chat])
                #:host (current-host)
                #:port (current-port)
                #:tpl (current-tpl)
                #:prefix (current-path-prefix))

  (define-runtime-module-path macpaste "macpaste.rkt")
  (define-runtime-path init-file "init.rkt")

  (when (current-native-paste)
    (case (system-type 'os)
      [(macosx) (dynamic-require macpaste #f)]
      [else (void)]))
  (unless (current-no-init)
    (namespace-require (list 'file (path->string init-file)) ns))
  
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

  (define running? (make-parameter #t))

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
         (current-pasted (list (car (regexp-split multi-input-paste-end (port->string in)))))
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
      (define (repl-read)
        (define v (parameterize ([running? #f])
                    (expeditor-read ee #:prompt ((current-repl-prompt)))))
        (cond
          [(eof-object? v) v]
          [(refreshing? v) (repl-read)]
          [(message? v)
           (message-content v)]
          [(cmd? v)
           (λ () (system (cmd-content v)))]
          [else
           (λ ()
             (call-with-values
              (λ () (eval v ns))
              (λ r
                (for ([v (in-list r)]
                      #:unless (void? v))
                  (println v)))))]))
      (current-repl-read repl-read)
      ((current-repl-loop)))
    (void (expeditor-close ee))))
