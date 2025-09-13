#lang typed/racket/base/shallow
(require "main.rkt"
         "types.rkt"
         racket/match
         racket/string
         racket/port)
(provide chat completion)

(define (build-oai-compat-message [message : Msg]) : (Immutable-HashTable Symbol JSExpr)
  (match-define (Msg role content tool-calls tool-call-id reasoning-content) message)
  (hash-build
   'role role
   'tool_calls
   (null->nullable
    (map (λ ([tc : ToolCall])
           (match-define (ToolCall name arguments id) tc)
           (hash-build
            'type "function"
            'id id
            'function
            (hasheq 'name name 'arguments arguments)))
         tool-calls))
   'tool_call_id (false->nullable tool-call-id)
   'content
   (cond
     [(string? content) content]
     [else
      (map (λ ([item : (U String Image)])
             (cond
               [(string? item)
                (hasheq 'type "text" 'text item)]
               [else
                (define d (string-append "data:image/jpeg;base64,"
                                         (bytes->string/latin-1 (base64-encode (Image-data item) #""))))
                (hasheq 'type "image_url" 'image_url (hasheq 'url d))]))
           content)])
   (current-reasoning-delta) (false->nullable reasoning-content)))

(define (build-body-common [options : Options])
  (hash-build
   'temperature (Options-temperature options)
   'top_k (Options-top-k options)
   'top_p (Options-top-p options)
   'min_p (Options-min-p options)
   'repeat_penalty (Options-repeat-penalty options)
   'stream (merge-right #t (Options-stream options))
   'stream_options (if (Options-stream options)
                       (hasheq 'include_usage #t)
                       'null)
   'max_tokens (Options-max-tokens options)
   'stop (null->nullable (Options-stop options))
   'grammar (Options-grammar options)))

(define (build-chat-body [messages : History] [options : Options])
  (jsexpr->bytes
   (hash-add (build-body-common options)
             'messages (map build-oai-compat-message messages)
             'tools (null->nullable (map Tool-desc (Options-tools options))))))

(define (parse-content-type-streaming? [headers : (Listof Bytes)])
  (let/ec k : Boolean
    (for ([line (in-list headers)])
      (define l (string-locale-downcase (bytes->string/utf-8 line)))
      (when (string-contains? l "content-type:")
        (cond
          [(string-contains? l "application/json") (k #f)]
          [(string-contains? l "text/event-stream") (k #t)]
          [else (error 'parse-content-type-stream "invalid: ~a" l)])))
    (error 'parse-content-type-stream "no content-type")))

(define (log-verbose [j : JSExpr])
  (when (current-verbose)
    (match j
      [(hash* ['timings (hash* ['prompt_n pp] ['prompt_per_second ppt]
                               ['predicted_n tg] ['predicted_per_second tgt])])
       (call/color
        'blue
        (λ ()
          (printf "PROMPT\t~a tokens, ~a tokens/s~%" pp ppt)
          (printf "EVAL\t~a tokens, ~a tokens/s" tg tgt)))]
      [(hash* ['usage (hash* ['prompt_tokens pp] ['completion_tokens tg])])
       (call/color
        'blue
        (λ ()
          (printf "PROMPT\t~a tokens~%" pp)
          (printf "EVAL\t~a tokens" tg)))]
      [else (void)])))

(define (handle-json-body [port : Input-Port] [streaming : Streaming]) : Msg
  (define s (port->bytes port))
  ((current-network-trace) 'recv s)
  (define j (bytes->jsexpr s))
  (log-verbose j)
  (match (json-ref j 'choices 0 'message)
    [(hash* ['content content #:default ""]
            ['tool_calls (? list? tool-calls) #:default '()]
            [(current-reasoning-delta) reasoning-content #:default #f])
     (define str-content
       (match content
         [(? string?) content]
         ['null ""]))
     (when (string? reasoning-content)
       (streaming reasoning-content 'think))
     (streaming str-content 'content)
     (Msg "assistant" str-content
          (map (λ ([tc : JSExpr]) ToolCall
                 (match tc
                   [(hash* ['id (? string? id) #:default ""]
                           ['function (hash*
                                       ['name (? string? name)]
                                       ['arguments (? string? arguments)])])
                    (ToolCall name arguments id)]))
               tool-calls)
          #f
          (and (string? reasoning-content) reasoning-content))]))

(define (on-event-stream [port : Input-Port] [handler : (-> JSExpr Void)])
  (let loop : Void ()
    (define l (read-line port 'any))
    (unless (eof-object? l)
      ((current-network-trace) 'recv l))
    (cond
      [(eof-object? l) (void)]
      [(not (non-empty-string? l)) (loop)]
      [(string=? l "data: [DONE]") (void)]
      [(string-prefix? l "data: ")
       (handler (string->jsexpr (substring l 6)))
       (loop)]
      [else (loop)])))

(define (handle-event-stream [port : Input-Port] [streaming : Streaming])
  (define whole-content (open-output-string))
  (define reasoning-content (open-output-string))
  (define streaming-tools : (Mutable-HashTable Integer (Option ToolCall)) (make-hasheqv))
  (define (merge-streaming-tools [a : (Option ToolCall)] [b : ToolCall])
    (match* (a b)
      [((ToolCall na aa ia) (ToolCall nb ab ib))
       (ToolCall (string-append na nb) (string-append aa ab) (string-append ia ib))]
      [(#f b) b]))
  (define (handle [j : JSExpr]) : Void
    (log-verbose j)
    (define delta (json-ref j 'choices 0 'delta))
    (match delta
      [(hash* ['content (? string? content)])
       (write-string content whole-content)
       (streaming content 'content)]
      [_ (void)])
    (match delta
      [(hash* [(current-reasoning-delta) (? string? content)])
       (write-string content reasoning-content)
       (streaming content 'think)]
      [_ (void)])
    (match delta
      [(hash* ['tool_calls
               (list (hash* ['index (? exact-integer? index) #:default 0]
                            ['id (? string? id) #:default ""]
                            ['function
                             (hash* ['name (? string? name) #:default ""]
                                    ['arguments (? string? arguments) #:default ""])]))])
       ((inst hash-update! Integer (Option ToolCall))
        streaming-tools index
        (λ ([tc : (Option ToolCall)])
          (merge-streaming-tools tc (ToolCall name arguments id)))
        (λ () #f))]
      [_ (void)])
    (void))
  (call/interrupt
   (λ ()
     (on-event-stream port handle))
   (λ ()
     (hash-clear! streaming-tools)))
  (Msg "assistant" (get-output-string whole-content)
       (for/list ([tc (in-hash-values streaming-tools)]
                  #:when tc)
         tc)
       #f
       (let ([s (get-output-string reasoning-content)])
         (if (non-empty-string? s) s #f))))

(define (chat [msgs : History] [streaming : Streaming] [opt : Options])
  (define data (build-chat-body msgs opt))
  ((current-network-trace) 'send data)
  (define-values (status headers body)
    (http-sendrecv/url (string->url (cast (Options-endpoint opt) String))
                       #:method "POST"
                       #:headers (cons "Content-Type: application/json" (Options-headers opt))
                       #:data data))
  (unless (string-contains? (bytes->string/latin-1 status) "200")
    (error 'send "status: ~a" status))
  (define streaming? (parse-content-type-streaming? headers))
  (begin0
    (cond
      [(not streaming?) (handle-json-body body streaming)]
      [else (handle-event-stream body streaming)])
    (close-input-port body)))

(define (build-completion-body [prompt : String] [options : Options])
  (jsexpr->bytes
   (hash-set (build-body-common options) 'prompt prompt)))

(define (completion [prompt : String] [streaming : Streaming] [opt : Options])
  (define data (build-completion-body prompt opt))
  ((current-network-trace) 'send data)
  (define-values (status headers body)
    (http-sendrecv/url (string->url (cast (Options-endpoint opt) String))
                       #:method "POST"
                       #:headers (cons "Content-Type: application/json" (Options-headers opt))
                       #:data data))
  (unless (string-contains? (bytes->string/latin-1 status) "200")
    (error 'send "status: ~a" status))
  (define streaming? (parse-content-type-streaming? headers))
  (define (handle [j : JSExpr])
    (log-verbose j)
    (define text (cast (json-ref j 'choices 0 'text) String))
    (streaming text 'content)
    (write-string text all-text)
    (void))
  (define all-text (open-output-string))
  (cond
    [(not streaming?)
     (define s (port->bytes body))
     ((current-network-trace) 'recv s)
     (define j (bytes->jsexpr s))
     (handle j)]
    [else
     (call/interrupt
      (λ ()
        (on-event-stream body handle))
      void)])
  (close-input-port body)
  (get-output-string all-text))