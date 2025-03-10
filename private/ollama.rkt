#lang typed/racket/base/shallow
(require "main.rkt"
         racket/match
         racket/string
         typed/json
         typed/net/base64
         typed/net/url)
(provide chat completion)

(define (build-ollama-message [message : Msg]) : (Immutable-HashTable Symbol JSExpr)
  (match-define (Msg role content images tool-calls _) message)
  (hash-build
   'role role
   'tool_calls
   (null->false
    (map (λ ([tc : ToolCall])
           (match-define (ToolCall name arguments id) tc)
           (hash-build
            'type "function"
            'id id
            'function
            (hasheq 'name name 'arguments (string->jsexpr arguments))))
         tool-calls))
   'content content
   'images
   (null->false
    (map (λ ([img : Bytes]) (bytes->string/latin-1 (base64-encode img))) images))))

(define (build-body-common [options : Options])
  (: h (Immutable-HashTable Symbol JSExpr))
  (define h
    (hash-build 'temperature (Options-temperature options)
                'top_k (Options-top-k options)
                'top_p (Options-top-p options)
                'min_p (Options-min-p options)
                'repeat_penalty (Options-repeat-penalty options)
                'num_ctx (Options-context-window options)
                'num_predict (Options-max-tokens options)
                'stop (null->false (Options-stop options))))
  (define s (Options-stream options))
  (define really-stream? (if s (unbox s) #t))
  (hash-build
   'options h
   'stream really-stream?
   'model (Options-model options)))

(define (build-chat-body [messages : History] [options : Options])
  (jsexpr->bytes
   (hash-and (build-body-common options)
             'messages (map build-ollama-message messages)
             'tools (null->false (Options-tools options)))))


(define (chat [msgs : History] [streaming : (String -> Void)] [opt : Options])
  (define data (build-chat-body msgs opt))
  ((current-network-trace) 'send data)
  (define-values (status headers body)
    (http-sendrecv/url (string->url (cast (Options-endpoint opt) String))
                       #:method "POST"
                       #:headers (cons "Content-Type: application/json" (Options-headers opt))
                       #:data data))
  (unless (string-contains? (bytes->string/latin-1 status) "200")
    (error 'send "status: ~a" status))
  (define tools : (Listof ToolCall) '())
  (define output-content (open-output-string))
  (call/interrupt
   (λ ()
     (let loop ()
       (define l (read-line body 'any))
       (cond
         [(eof-object? l) (void)]
         [else
          ((current-network-trace) 'recv l)
          (define j (string->jsexpr l))
          (match j
            [(hash* ['message (hash* ['tool_calls (? list? tool-calls)])])
             (for-each
              (λ ([tc : JSExpr])
                (match tc
                  [(hash* ['function (hash* ['name (? string? name)]
                                            ['arguments arguments])])
                   (set! tools (cons (ToolCall name (jsexpr->string arguments) "") tools))]))
              tool-calls)]
            [else (void)])
          (match j
            [(hash* ['message (hash* ['content (? string? content)])])
             (write-string content output-content)
             (streaming content)]
            [(hash* ['error err])
             (error 'chat "~a" err)])
          (loop)])))
   void)
  (close-input-port body)
  (Msg "assistant" (get-output-string output-content) '() (reverse tools) #f))

(define (build-completion-body [prompt : String] [options : Options])
  (jsexpr->bytes
   (hash-and (build-body-common options)
             'prompt prompt
             'raw #t)))

(define (completion [prompt : String] [streaming : (String -> Void)] [opt : Options])
  (define data (build-completion-body prompt opt))
  ((current-network-trace) 'send data)
  (define-values (status headers body)
    (http-sendrecv/url (string->url (cast (Options-endpoint opt) String))
                       #:method "POST"
                       #:headers (cons "Content-Type: application/json" (Options-headers opt))
                       #:data data))
  (unless (string-contains? (bytes->string/latin-1 status) "200")
    (error 'send "status: ~a" status))
  (define output-content (open-output-string))
  (let loop ()
    (define l (read-line body 'any))
    (cond
      [(eof-object? l) (void)]
      [else
       ((current-network-trace) 'recv l)
       (define j (string->jsexpr l))
       (match j
         [(hash* ['response (? string? content)])
          (write-string content output-content)
          (streaming content)]
         [(hash* ['error err])
          (error 'chat "~a" err)])
       (loop)]))
  (close-input-port body)
  (get-output-string output-content))