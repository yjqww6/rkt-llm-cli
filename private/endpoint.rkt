#lang typed/racket/base/shallow
(require "main.rkt"
         "chat.rkt"
         "chat-template.rkt"
         (prefix-in oai: "oai-compat.rkt")
         (prefix-in response: "oai-response.rkt"))
(provide (all-defined-out))

(define-type BackendType (U 'oai-completion 'oai-chat 'oai-response))

(define current-chatter (make-parameter (ann (λ (h s o) (error 'chatter "no endpoint")) Chatter)))
(define current-interactive-chatter (make-parameter (ann (λ (h s o) (error 'interactive-chatter "no endpoint")) InteractiveChatter)))

(define (make-default-options [host : String] [port : Exact-Nonnegative-Integer] [path : String] [model : String])
  (make-Options
   #:endpoint (format "http://~a:~a/~a" host port path)
   #:model model))

(define default-chatter-options (make-parameter (make-Options)))

(define (endpoint #:type [type : BackendType 'oai-chat]
                  #:host [host : String "localhost"] #:port [port : (Option Exact-Nonnegative-Integer) #f]
                  #:prefix [prefix : (Option String) #f] #:model [model : String "default_model"])
  (make-default-options host
                        (or port 9931)
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
     (current-chatter response:chat)
     (current-interactive-chatter
      (make-default-interactive-chatter (λ (h s o) ((current-chatter) h s o)) default-chatter-options))]
    [(eq? type 'oai-chat)
     (current-chatter oai:chat)
     (current-interactive-chatter
      (make-default-interactive-chatter (λ (h s o) ((current-chatter) h s o)) default-chatter-options))]
    [(eq? type 'oai-completion)
     (current-chatter (make-chat-by-template (λ (h s o) (oai:completion h s o)) (chat-template (or tpl "chatml"))))
     (current-interactive-chatter
      (make-default-interactive-chatter (λ (h s o) ((current-chatter) h s o)) default-chatter-options))]))

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

(define current-chatter-hooks (make-parameter (ann '() (Listof (History Streaming Options Chatter -> Msg)))))
(define (with-chatter-hooks [chatter : Chatter]) : Chatter
  (λ (h s o)
    (define hooks (current-chatter-hooks))
    (let f ([hooks hooks] [h h] [s s] [o o])
      (cond
        [(null? hooks) (chatter h s o)]
        [else ((car hooks) h s o (λ (h s o) (f (cdr hooks) h s o)))]))))

(define (make-default-interactive-chatter [chatter : Chatter] [default-opts : (Parameterof Options)]) : InteractiveChatter
  (define new-chatter
    (with-autosave
        (with-total-tokens
            (make-interactive-chat
             (with-messages-postprocessor
                 (with-messages-preprocessor
                     (with-chatter-hooks chatter)))))))
  (λ (i s o)
    (new-chatter i s (merge-Options (default-opts) o))))