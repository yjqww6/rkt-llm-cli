#lang typed/racket/base/shallow
(require "private/main.rkt"
         (prefix-in oai: "private/oai-compat.rkt")
         (prefix-in ollama: "private/ollama.rkt")
         "private/chat.rkt"
         "private/chat-template.rkt"
         racket/match
         racket/list
         typed/json
         typed/net/url)
(provide (all-defined-out)
         (all-from-out "private/main.rkt")
         (all-from-out "private/chat.rkt"))

(define-type Chat (-> Interactive Void))
(: make-default-chat (-> InteractiveChatter Options Chat))
(define (make-default-chat chatter default-options)
  (define new-chatter (with-interactive-preprocessor chatter))
  (λ (s)
    (chatter s
             (λ (s) (display s) (flush-output))
             (merge-Options default-options (current-Options)))
    (newline)))

(define current-messages-preprocessors (make-parameter (ann '() (Listof (-> History History)))))
(define (with-messages-preprocessor [chatter : Chatter]) : Chatter
  (map-chatter chatter
               (λ (h) (((inst foldl (-> History History) (-> History History))
                        compose1 values (current-messages-preprocessors)) h))))

(define current-interactive-preprocessors (make-parameter (ann '() (Listof (-> Interactive Interactive)))))
(define (with-interactive-preprocessor [chatter : InteractiveChatter]) : InteractiveChatter
  (map-interactive-chatter
   chatter
   (λ (s) (((inst foldl (-> Interactive Interactive) (-> Interactive Interactive))
            compose1 values (current-interactive-preprocessors)) s))))

(define llama-cpp-chat-default-options
  (make-Options
   #:endpoint "http://localhost:8080/v1/chat/completions"))

(define llama-cpp-completion-default-options
  (make-Options
   #:endpoint "http://localhost:8080/v1/completions"))

(define lmstudio-chat-default-options
  (make-Options
   #:endpoint "http://localhost:1234/v1/chat/completions"))

(define lmstudio-completion-default-options
  (make-Options
   #:endpoint "http://localhost:1234/v1/completions"))

(define (make-default-options [host : String] [port : Exact-Nonnegative-Integer] [path : String])
  (make-Options
   #:endpoint (format "http://~a:~a/~a" host port path)))

(define (oai-compat-chat [default-options : Options llama-cpp-chat-default-options])
  (make-default-chat (make-interactive-chat (with-messages-preprocessor oai:chat)) default-options))

(define (oai-compat-completion
         [tpl : (U String ChatTemplate)]
         [default-options : Options llama-cpp-completion-default-options]) : Chat
  (define chat-tpl (if (string? tpl) (chat-template tpl) tpl))
  (make-default-chat (make-interactive-chat (with-messages-preprocessor (make-chat-by-template oai:completion chat-tpl)))
                     default-options))

(define (ollama-chat [default-options : Options])
  (make-default-chat (make-interactive-chat (with-messages-preprocessor ollama:chat)) default-options))

(define (ollama-completion [tpl : (U String ChatTemplate)] [default-options : Options])
  (define chat-tpl (if (string? tpl) (chat-template tpl) tpl))
  (make-default-chat (make-interactive-chat (with-messages-preprocessor (make-chat-by-template ollama:completion chat-tpl)))
                     default-options))

(define current-chat (make-parameter (oai-compat-chat)))

(define #:forall (a) (call-with-cust (thunk : (-> a)))
  (define cust (make-custodian))
  (dynamic-wind void
                (λ ()
                  (parameterize ([current-custodian cust])
                    (thunk)))
                (λ () (custodian-shutdown-all cust))))

(: chat Chat)
(define (chat s)
  (call-with-cust
   (λ ()
     ((current-chat) s))))

(define ((oai-compat-complete [default-options : Options llama-cpp-completion-default-options])
         [s : String])
  (oai:completion s (λ (s) (display s) (flush-output))
                  (merge-Options default-options (current-Options)))
  (void))

(define ((ollama-complete [default-options : Options]) [s : String])
  (ollama:completion s (λ (s) (display s) (flush-output))
                     (merge-Options default-options (current-Options)))
  (void))

(define current-complete (make-parameter (ann (λ (s) (error 'current-complete))
                                              (-> String Void))))
(define (complete [s : String])
  (call-with-cust
   (λ ()
     ((current-complete) s))))

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
  (match/values
   (split-at-right (current-history) 2)
   [(history (list (struct* Msg ([role "user"] [content content])) assistant))
    (match (regexp-match #px"^```\n(.*)\n```(.*)$" content)
      [(list _ paste _)
       (current-paste-text paste)
       (current-history history)])]))

(define current-repl-loop (make-parameter (ann (λ () (error 'repl-loop)) (-> Any))))
(define (repl-loop) ((current-repl-loop)))