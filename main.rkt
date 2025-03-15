#lang typed/racket/base/shallow
(require "private/main.rkt"
         (prefix-in oai: "private/oai-compat.rkt")
         (prefix-in ollama: "private/ollama.rkt")
         "private/chat.rkt"
         "private/chat-template.rkt"
         racket/match
         racket/list
         racket/port)
(provide (all-defined-out)
         (all-from-out "private/main.rkt")
         (all-from-out "private/chat.rkt"))

(define current-chatter (make-parameter (ann (λ (h s o) (error 'chatter "no endpoint")) InteractiveChatter)))
(define current-completer (make-parameter (ann (λ (h s o) (error 'completer "no endpoint")) Completer)))
(define default-chatter-options (make-parameter (make-Options)))
(define default-complete-options (make-parameter (make-Options)))

(define current-messages-preprocessors (make-parameter (ann '() (Listof (-> History History)))))
(define (with-messages-preprocessor [chatter : Chatter]) : Chatter
  (map-chatter chatter
               (λ (h) (((inst foldl (-> History History) (-> History History))
                        compose1 values (current-messages-preprocessors)) h))))

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

(define (make-default-interactive-chatter [chatter : Chatter]) : InteractiveChatter
  (define new-chatter (make-interactive-chat (with-messages-preprocessor chatter)))
  (λ (i s o)
    (new-chatter i s (merge-Options (default-chatter-options) o))))

(define (make-default-completion-interactive-chatter [complete : Completer] [tpl : (U String ChatTemplate)]) : InteractiveChatter
  (define chat-tpl (if (string? tpl) (chat-template tpl) tpl))
  (define new-chatter (make-interactive-chat (with-messages-preprocessor (make-chat-by-template complete chat-tpl))))
  (λ (i s o)
    (new-chatter i s (merge-Options (default-complete-options) o))))

(define oai-compat-chat
  (make-default-interactive-chatter oai:chat))

(define (oai-compat-completion
         [tpl : (U String ChatTemplate)]) : InteractiveChatter
  (make-default-completion-interactive-chatter oai:completion tpl))

(define ollama-chat
  (make-default-interactive-chatter ollama:chat))

(define (ollama-completion [tpl : (U String ChatTemplate)])
  (make-default-completion-interactive-chatter ollama:completion tpl))


(define-type Chat (-> Interactive Void))
(define default-chat : Chat
  (λ (s)
    (define new-chatter (with-interactive-hooks (current-chatter)))
    (new-chatter s (λ (s) (display s) (flush-output)) (current-Options))
    (newline)))

(define-type Complete (-> String Void))
(define default-complete : Complete
  (λ (s)
    ((current-completer) s (λ (s) (display s) (flush-output)) (current-Options))
    (newline)))

(define (make-default-options [host : String] [port : Exact-Nonnegative-Integer] [path : String])
  (make-Options
   #:endpoint (format "http://~a:~a/~a" host port path)))

(define #:forall (a) (call-with-cust (thunk : (-> a)))
  (define cust (make-custodian))
  (dynamic-wind void
                (λ ()
                  (parameterize ([current-custodian cust])
                    (thunk)))
                (λ () (custodian-shutdown-all cust))))

(define current-chat (make-parameter default-chat))
(define current-complete (make-parameter default-complete))

(define (chat [s : Interactive])
  (call-with-cust
   (λ ()
     ((current-chat) s))))

(define (complete [s : String])
  (call-with-cust
   (λ ()
     ((current-complete) s))))

(define (use-endpoint #:type [type 'oai-compat] #:host [host : String "localhost"] #:port [port : (Option Exact-Nonnegative-Integer) #f]
                      #:tpl [tpl : (Option String) #f] #:prefix [prefix : String "v1/"])
  (cond
    [(eq? type 'oai-compat)
     (default-complete-options (make-default-options host (or port 8080) (string-append prefix "completions")))
     (default-chatter-options (make-default-options host (or port 8080) (string-append prefix "chat/completions")))
     (current-chatter (if tpl (oai-compat-completion tpl) oai-compat-chat))
     (current-completer oai:completion)]
    [(eq? type 'ollama)
     (default-complete-options (make-default-options host (or port 11434) "api/generate"))
     (default-chatter-options (make-default-options host (or port 11434) "api/chat"))
     (current-chatter (if tpl (ollama-completion tpl) ollama-chat))
     (current-completer ollama:completion)]))

(define current-paste-text (make-parameter (ann #f (Option String))))
(define current-paste-image (make-parameter (ann #f (Option Bytes))))
(define current-output-prefix (make-parameter (ann #f (Option String))))
(define (repl-chat [prompt : (U String 'redo 'continue)])
  (cond
    [(or (eq? prompt 'redo) (eq? prompt 'continue)) (chat prompt)]
    [else
     (define text (current-paste-text))
     (define image (current-paste-image))
     (define prefix (current-output-prefix))
     (define content (if text (string-append "```\n" text "\n```\n" prompt) prompt))
     (define user (Msg "user" content (if image (list image) '()) '() #f))
     (if prefix
         (chat (cons user prefix))
         (chat user))
     (current-paste-text #f)
     (current-paste-image #f)
     (current-output-prefix #f)]))

(define (default-repl-prompt)
  (string-append
   (if (current-paste-text) "text " "")
   (if (current-paste-image) "img " "")
   (if (current-output-prefix) "pre " "")
   ">>>"))
(define current-repl-prompt (make-parameter default-repl-prompt))

(define (take-last-prompt)
  (define (extract-paste-text [content : String])
    (match (regexp-match #px"^```\n(.*)\n```(.*)$" content)
      [(list _ paste _) paste]
      [_ #f]))
  (match/values
   (split-at-right (current-history) 2)
   [(history (list (struct* Msg ([role "user"] [content (app extract-paste-text paste)] [images images])) assistant))
    #:when (or paste (not (null? images)))
    (when paste
      (current-paste-text paste))
    (unless (null? images)
      (current-paste-image (car images)))
    (current-history history)]))

(define current-repl-loop (make-parameter (ann (λ () (error 'repl-loop)) (-> Any))))
(define (repl-loop) ((current-repl-loop)))

(define (use-prefix-by-grammar!)
  (: hook InteractiveHook)
  (define (hook h o)
    (if (interactive-is-user&prefill? h)
        (values (car h)
                (struct-copy Options o [grammar (format "root ::= ~v .*" (cdr h))]))
        (values h o)))
  (current-interactive-hooks (cons hook (current-interactive-hooks))))

(define (trace-network! [output : Output-Port (current-output-port)])
  (current-network-trace
   (lambda (type data)
     (fprintf output "~a: ~a~%" type data))))

(define (untrace-network!)
  (current-network-trace void))

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
  (Msg-content (last (current-history))))