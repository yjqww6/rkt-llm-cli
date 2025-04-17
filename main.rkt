#lang typed/racket/base/shallow
(require "private/main.rkt"
         (prefix-in oai: "private/oai-compat.rkt")
         (prefix-in ollama: "private/ollama.rkt")
         "private/chat.rkt"
         "private/chat-template.rkt"
         racket/match
         racket/list
         racket/string
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

(define (make-default-interactive-chatter [chatter : Chatter]) : InteractiveChatter
  (define new-chatter (make-interactive-chat (with-messages-postprocessor (with-messages-preprocessor chatter))))
  (λ (i s o)
    (new-chatter i s (merge-Options (default-chatter-options) o))))

(define (make-default-completion-interactive-chatter [complete : Completer] [tpl : (U String ChatTemplate)]) : InteractiveChatter
  (define chat-tpl (if (string? tpl) (chat-template tpl) tpl))
  (define new-chatter (make-interactive-chat (with-messages-postprocessor (with-messages-preprocessor (make-chat-by-template complete chat-tpl)))))
  (λ (i s o)
    (new-chatter i s (merge-Options (default-complete-options) o))))

(define-type Chat (-> Interactive Void))
(define default-chat : Chat
  (λ (s)
    (define new-chatter (with-interactive-hooks (current-chatter)))
    (new-chatter s ((current-streaming)) (current-Options))
    (newline)))

(define-type Complete (-> String Void))
(define default-complete : Complete
  (λ (s)
    ((current-completer) s ((current-streaming)) (current-Options))
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

(define-type BackendType (U 'oai-compat 'ollama))

(define (endpoint #:type [type : BackendType 'oai-compat] #:complete? [complete? : Boolean #f]
                  #:host [host : String "localhost"] #:port [port : (Option Exact-Nonnegative-Integer) #f]
                  #:prefix [prefix : (Option String) #f])
  (make-default-options host
                        (or port (cond
                                   [(eq? type 'oai-compat) 8080]
                                   [(eq? type 'ollama) 11434]))
                        (string-append (or prefix
                                           (cond
                                             [(eq? type 'oai-compat) "v1/"]
                                             [(eq? type 'ollama) ""]))
                                       (cond
                                         [(eq? type 'oai-compat) (if complete? "completions" "chat/completions")]
                                         [(eq? type 'ollama) (if complete? "api/generate" "api/chat")]))))

(define (new-chatter #:type [type : BackendType 'oai-compat]) : Chatter
  (cond
    [(eq? type 'oai-compat) oai:chat]
    [(eq? type 'ollama) ollama:chat]))

(define (new-completer #:type [type : BackendType 'oai-compat]) : Completer
  (cond
    [(eq? type 'oai-compat) oai:completion]
    [(eq? type 'ollama) ollama:completion]))

(define (use-endpoint #:type [type : BackendType 'oai-compat]
                      #:host [host : String "localhost"] #:port [port : (Option Exact-Nonnegative-Integer) #f]
                      #:tpl [tpl : (Option String) #f] #:prefix [prefix : (Option String) #f])
  (default-complete-options (endpoint #:type type #:host host #:port port #:prefix prefix #:complete? #t))
  (default-chatter-options (endpoint #:type type #:host host #:port port #:prefix prefix #:complete? (and tpl #t)))
  (current-completer (new-completer #:type type))
  (current-chatter (if tpl
                       (make-default-completion-interactive-chatter (new-completer #:type type) tpl)
                       (make-default-interactive-chatter (new-chatter #:type type)))))

(define current-paste-text (make-parameter (ann '() (Listof String))))
(define current-paste-image (make-parameter (ann '() (Listof Image))))
(define current-output-prefix (make-parameter (ann #f (Option String))))
(define (repl-chat [prompt : (U String 'redo 'continue (Listof (U String Image)))])
  (call-with-continuation-prompt
   (λ ()  
     (cond
       [(or (eq? prompt 'redo) (eq? prompt 'continue)) (chat prompt)]
       [else
        (define texts (current-paste-text))
        (define images (current-paste-image))
        (define prefix (current-output-prefix))
        (define pasted-text
          (if (null? texts) #f
              (string-join texts "\n" #:before-first "```\n" #:after-last "\n```\n")))
        (define user
          (cond
            [(string? prompt)
             (define content (if pasted-text
                                 (string-append pasted-text prompt)
                                 prompt))
             (Msg "user" (if (null? images)
                             content
                             (append images (list content))) '() #f)]
            [else
             (Msg "user"
                  (append images
                          (if pasted-text (list pasted-text) '())
                          prompt)
                  '()
                  #f)]))
        (if prefix
            (chat (cons user prefix))
            (chat user))
        (current-paste-text '())
        (current-paste-image '())
        (current-output-prefix #f)]))
   break-prompt-tag
   (λ ([cc : (-> Nothing)])
     (newline)
     (display "Accept as history[y/n]:")
     (define line (read-line (current-input-port) 'any))
     (when (and (not (eof-object? line)) (regexp-match? #px"^\\s*y" line))
       (cc)))))

(: do-paste (case->
             [String Boolean -> Void]
             [Image Boolean -> Void]))
(define (do-paste pasted append?)
  (define param (if (string? pasted) current-paste-text current-paste-image))
  (cond
    [append? (param (append (param) (list pasted)))]
    [else (param (list pasted))]))

(define (default-repl-prompt)
  (with-handlers* ([exn:fail? (λ (e) ">>>")])
    (string-append
     (if (not (null? (current-paste-text))) "text " "")
     (if (not (null? (current-paste-image))) "img " "")
     (if (current-output-prefix) "pre " "")
     ">>>")))
(define current-repl-prompt (make-parameter default-repl-prompt))

(define (take-last-prompt)
  (define (extract-paste-text [content : String])
    (match (regexp-match #px"^```\n(.*)\n```(.*)$" content)
      [(list _ paste _) paste]
      [_ #f]))
  (define (extract-paste [content : (U String (Listof (U Image String)))])
    (cond
      [(string? content) (extract-paste-text content)]
      [else
       (define item (last content))
       (cond
         [(string? item) (extract-paste-text item)]
         [else #f])]))
  (define (extract-imgs [content : (U String (Listof (U Image String)))]) : (Listof Image)
    (cond
      [(string? content) '()]
      [else (filter-map (λ ([item : (U Image String)]) (and (Image? item) item)) content)]))
  (match/values
   (split-at-right (current-history) 2)
   [(history (list (struct* Msg ([role "user"] [content (and (app extract-paste paste) (app extract-imgs images))])) assistant))
    #:when (or paste (not (null? images)))
    (when paste
      (current-paste-text (list paste)))
    (unless (null? images)
      (current-paste-image images))
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

(define (trace-network! [only : (U 'send 'recv #f) #f] [output : Output-Port (current-output-port)])
  (current-network-trace
   (lambda (type data)
     (when (or (not only) (eq? type only))
       (fprintf output "~a: ~a~%" type data)))))

(define (untrace-network!)
  (current-network-trace void))

(define (warmup)
  (parameterize ([current-output-prefix (current-output-prefix)]
                 [current-paste-text (current-paste-text)]
                 [current-paste-image (current-paste-image)]
                 [current-max-tokens 1]
                 [current-stream #f]
                 [current-history (current-history)]
                 [current-output-port (open-output-nowhere)])
    (repl-chat "")))

(define (redo)
  (repl-chat 'redo))

(define (last-response)
  (Msg-content (last (current-history))))