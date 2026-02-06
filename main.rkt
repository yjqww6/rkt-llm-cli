#lang typed/racket/base/shallow
(require "private/main.rkt"
         (prefix-in oai: "private/oai-compat.rkt")
         (prefix-in response: "private/oai-response.rkt")
         "private/chat.rkt"
         "private/chat-template.rkt"
         racket/match
         racket/list
         racket/string
         racket/port)
(provide (all-defined-out)
         (all-from-out "private/main.rkt")
         (all-from-out "private/chat.rkt")
         response:use-response-id)

(define current-interactive-chatter (make-parameter (ann (λ (h s o) (error 'interactive-chatter "no endpoint")) InteractiveChatter)))
(define current-chatter (make-parameter (ann (λ (h s o) (error 'chatter "no endpoint")) Chatter)))
(define default-chatter-options (make-parameter (make-Options)))

(define current-messages-preprocessors (make-parameter (ann '() (Listof (-> History History)))))
(define (with-messages-preprocessor [chatter : Chatter]) : Chatter
  (map-chatter chatter
               (λ (h) (((inst foldl (-> History History) (-> History History))
                        compose1 values (current-messages-preprocessors)) h))))
(define current-messages-postprocessors (make-parameter (ann '() (Listof (-> Msg Msg)))))
(define (with-messages-postprocessor [chatter : Chatter]) : Chatter
  (λ (h s o)
    (define m (chatter h s o))
    (((inst foldl (-> Msg Msg) (-> Msg Msg))
      compose1 values (current-messages-postprocessors)) m)))

(define current-interactive-hooks (make-parameter (ann '() (Listof InteractiveHook))))
(define (with-interactive-hooks [chatter : InteractiveChatter]) : InteractiveChatter
  (λ (h s o)
    (define-values (new-h new-o)
      (let loop : (Values Interactive Options)
        ([hooks (current-interactive-hooks)] [h h] [o o])
        (cond
          [(null? hooks) (values h o)]
          [else
           (define-values (new-h new-o)
             ((car hooks) h o))
           (loop (cdr hooks) new-h new-o)])))
    (chatter new-h s new-o)))

(define (make-default-interactive-chatter [chatter : Chatter] [default-opts : (Parameterof Options)]) : InteractiveChatter
  (define new-chatter (make-interactive-chat (with-messages-postprocessor (with-messages-preprocessor chatter))))
  (λ (i s o)
    (new-chatter i s (merge-Options (default-opts) o))))

(define-type Chat (-> Interactive Void))
(define default-chat : Chat
  (λ (s)
    (define new-chatter (with-interactive-hooks (current-interactive-chatter)))
    (new-chatter s ((current-streaming)) (current-Options))
    (newline)))

(define (make-default-options [host : String] [port : Exact-Nonnegative-Integer] [path : String] [model : String])
  (make-Options
   #:endpoint (format "http://~a:~a/~a" host port path)
   #:model model))

(define #:forall (a) (call-with-cust (thunk : (-> a)))
  (define cust (make-custodian))
  (dynamic-wind void
                (λ ()
                  (parameterize ([current-custodian cust])
                    (thunk)))
                (λ () (custodian-shutdown-all cust))))

(define current-chat (make-parameter default-chat))

(define (chat [s : Interactive])
  (call-with-cust
   (λ ()
     ((current-chat) s))))

(define-type BackendType (U 'oai-completion 'oai-chat 'oai-response))

(define (endpoint #:type [type : BackendType 'oai-chat]
                  #:host [host : String "localhost"] #:port [port : (Option Exact-Nonnegative-Integer) #f]
                  #:prefix [prefix : (Option String) #f] #:model [model : String "default_model"])
  (make-default-options host
                        (or port 8080)
                        (string-append (or prefix "v1/")
                                       (cond
                                         [(eq? type 'oai-chat) "chat/completions"]
                                         [(eq? type 'oai-completion) "completions"]
                                         [(eq? type 'oai-response) "responses"]))
                        model))

(define (use-endpoint #:type [type : BackendType 'oai-chat]
                      #:host [host : String "localhost"] #:port [port : (Option Exact-Nonnegative-Integer) #f]
                      #:tpl [tpl : (Option String) #f] #:prefix [prefix : (Option String) #f]
                      #:model [model : String "default_model"])
  (default-chatter-options (endpoint #:type type #:host host #:port port #:prefix prefix #:model model))
  (cond
    [(eq? type 'oai-response)
     (current-chatter (λ (h s o) (error 'use-endpoint "no chatter for responses")))
     (current-interactive-chatter
      (let ([chat response:chat])
        (ann
         (λ (h s o)
           (chat h s (merge-Options (default-chatter-options) o)))
         InteractiveChatter)))]
    [(eq? type 'oai-chat)
     (current-chatter oai:chat)
     (current-interactive-chatter
      (make-default-interactive-chatter (λ (h s o) ((current-chatter) h s o)) default-chatter-options))]
    [(eq? type 'oai-completion)
     (current-chatter (make-chat-by-template (λ (h s o) (oai:completion h s o)) (chat-template (or tpl "chatml"))))
     (current-interactive-chatter
      (make-default-interactive-chatter (λ (h s o) ((current-chatter) h s o)) default-chatter-options))]))

(define-parameter current-pasted '() : (Listof (U String Image)))
(define (clear-paste)
  (current-pasted '()))

(define pasted-history : (Thread-Cellof (Option (Weak-HashTable Msg (Listof (U String Image))))) (make-thread-cell #f #f))
(thread-cell-set! pasted-history (ann (make-weak-hasheq) (Weak-HashTable Msg (Listof (U String Image)))))

(define (find-new-user [h : History] [base : History]) : (Option Msg)
  (let loop ([h h] [b base])
    (match* (h b)
      [((cons _ h1) (cons _ b1)) (loop h1 b1)]
      [((cons (and u (struct* Msg ([role "user"]))) _) '()) u]
      [(_ _) #f])))

(define (remember-pasted [base : History] [h : History] [p : (Listof (U String Image))])
  (define w (thread-cell-ref pasted-history))
  (when w
    (define u (find-new-user h base))
    (when u
      (hash-set! w u p))))
(define (restore-pasted [base : History] [h : History]) : (Option (Listof (U String Image)))
  (define w (thread-cell-ref pasted-history))
  (and w
       (let ([u (find-new-user h base)])
         (and u
              (hash-ref w u (λ () #f))))))

(define-parameter current-output-prefix #f : (Option String))

(define (repl-chat [prompt : (U String 'redo 'continue (Listof (U String Image)))])
  (call-with-continuation-prompt
   (λ ()  
     (cond
       [(eq? prompt 'redo)
        (define prefix (current-output-prefix))
        (chat (Redo prefix))
        (current-output-prefix #f)]
       [(eq? prompt 'continue)
        (chat (Continue))]
       [else
        (define pasted (current-pasted))
        (define prefix (current-output-prefix))
        (define user
          (make-user (append (if (null? pasted)
                                 '()
                                 (append '("```") pasted '("```")))
                             (if (string? prompt) (list prompt) prompt))))
        (define base (current-history))
        (chat (User prefix user))
        (remember-pasted base (current-history) pasted)
        (current-pasted '())
        (current-output-prefix #f)]))
   break-prompt-tag
   (λ ([cc : (-> Nothing)])
     (newline)
     (display "Accept as history[y/n]:")
     (define line (read-line (current-input-port) 'any))
     (when (and (not (eof-object? line)) (regexp-match? #px"^\\s*y" line))
       (cc)))))

(define-parameter current-paste (λ () #f) : (-> (Option (Listof (U Image String)))))

(: do-paste (-> (Listof (U Image String)) Boolean Void))
(define (do-paste pasted append?)
  (cond
    [append? (current-pasted (append (current-pasted) pasted))]
    [else (current-pasted pasted)]))

(define (default-repl-prompt)
  (string-append
   (let ([c (count string? (current-pasted))])
     (if (> c 0) (format "text:~a " c) ""))
   (let ([c (count Image? (current-pasted))])
     (if (> c 0) (format "img:~a " c) ""))
   (if (current-output-prefix) "pre " "")
   ">>>"))
(define current-repl-prompt (make-parameter default-repl-prompt))

(define (make-prefix-repl-prompt [prefix : String])
  (define old (current-repl-prompt))
  (λ () (string-append prefix " " (old))))

(define (undo/pasted)
  (define h (current-history))
  (undo)
  (define base (current-history))
  (define r (restore-pasted base h))
  (when r
    (current-pasted r)))

(define current-repl-loop (make-parameter (ann (λ () (error 'repl-loop)) (-> Any))))
(define (repl-loop) ((current-repl-loop)))

(define (trace-network! [only : (U 'send 'recv #f) #f] [output : Output-Port (current-output-port)])
  (current-network-trace
   (lambda (type data)
     (when (or (not only) (eq? type only))
       (fprintf output "~a: ~a~%" type data)))))

(define (untrace-network!)
  (current-network-trace void))

(define (warmup)
  (parameterize ([current-output-prefix (current-output-prefix)]
                 [current-pasted (current-pasted)]
                 [current-max-tokens 1]
                 [current-stream #f]
                 [current-history (current-history)]
                 [current-output-port (open-output-nowhere)])
    (repl-chat "")))

(define (redo)
  (repl-chat 'redo))

(define (last-response)
  (Msg-content (last (current-history))))

(define (with-output-prefix [prefix : String]) : InteractiveHook
  (λ (i o)
    (values
     (cond
       [(User? i) (struct-copy User i [prefix #:parent InteractiveCommon prefix])]
       [(ToolResult? i) (struct-copy ToolResult i [prefix #:parent InteractiveCommon prefix])]
       [(Redo? i) (struct-copy Redo i [prefix #:parent InteractiveCommon prefix])]
       [else i])
     o)))

(define (with-user [f : (-> Msg Msg)]) : InteractiveHook
  (λ (i o)
    (values
     (cond
       [(User? i) (struct-copy User i [msg (f (User-msg i))])]
       [else i])
     o)))

(define (no-think)
  (parameterize ([current-interactive-hooks (list* (with-output-prefix "<think>\n\n</think>\n\n")
                                                   (with-user (λ (m) (merge-message m (make-user " /no_think"))))
                                                   (current-interactive-hooks))]
                 [current-repl-prompt (make-prefix-repl-prompt "NOTHK")])
    (repl-loop)))

(define (reroute-image [opt : Options])
  ;; oai-chat only for now
  (define old-chatter (current-chatter))
  (: new-chatter Chatter)
  (define (new-chatter h s o)
    (old-chatter h s
                 (if (for/or : Boolean ([m (in-list h)])
                       (and (not (string? (Msg-content m)))
                            (for/or : Boolean ([p (in-list (Msg-content m))])
                              (not (string? p)))))
                     (merge-Options o opt)
                     o)))
  (parameterize ([current-chatter new-chatter]
                 [current-repl-prompt (make-prefix-repl-prompt "REROUTE")])
    (repl-loop)))

(define (use-authorization-token [token : (Option String) #f])
  (define tok
    (or token (getenv "LLM_API_TOKEN")))
  (assert tok)
  (current-headers
   (cons (string-append "Authorization: Bearer " tok)
         (current-headers))))