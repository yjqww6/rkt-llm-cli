#lang typed/racket/base/shallow
(require "main.rkt"
         "types.rkt"
         "chat.rkt"
         racket/match
         racket/string
         racket/port)

(provide chat)

(define (build-messages [msg : Msg])
  (match-define (Msg role content _ id _) msg)
  (cond
    [(string=? role "user")
     (cond
       [(string? content) content]
       [else
        (list
         (hash-build
          'role role
          'content
          (map (λ ([item : (U String Image)])
                 (cond
                   [(string? item)
                    (hasheq 'type "input_text" 'text item)]
                   [else
                    (define d (string-append "data:image/jpeg;base64,"
                                             (bytes->string/latin-1 (base64-encode (Image-data item) #""))))
                    (hasheq 'type "input_image" 'image_url d)]))
               content)))])]
    [(string=? role "tool")
     (assert (string? content))
     (hash-build
      'type "function_call_output"
      'call_id id
      'role role
      'output content)]
    [else
     (error 'build-messages "unknown role: ~a" role)]))

(define (rewrite-tool [tool : JSExpr]) : JSExpr
  (match tool
    [(hash 'function func 'type "function" #:open)
     #:when (hash? func)
     (hash-set func 'type "function")]
    [_ (error 'rewrite-tool "invalid tool: ~a" tool)]))

(define (->msgs [interactive : Interactive]) : (Listof Msg)
  (cond
    [(User? interactive) (list (User-msg interactive))]
    [(ToolResult? interactive) (ToolResult-result interactive)]
    [else (error '->msgs "invalid interactive: ~a" interactive)]))

(define (build-oai-response-request [interactive : Interactive] [prev-resp-id : (Option String)] [options : Options]) : (Immutable-HashTable Symbol JSExpr)
  (define input : JSExpr
    (cond
      [(User? interactive) (build-messages (User-msg interactive))]
      [(ToolResult? interactive) (map build-messages (ToolResult-result interactive))]
      [else (error 'build-oai-response-request "invalid interactive: ~a" interactive)]))
  (hash-build
   'model (Options-model options)
   'input input
   'instructions (false->nullable (current-system))
   'stream (merge-right #t (Options-stream options))
   'tools (null->nullable (map rewrite-tool (map Tool-desc (Options-tools options))))
   'previous_response_id (false->nullable prev-resp-id)
   'temperature (Options-temperature options)
   'top_k (Options-top-k options)
   'top_p (Options-top-p options)
   'min_p (Options-min-p options)
   'repeat_penalty (Options-repeat-penalty options)
   'max_output_tokens (Options-max-tokens options)))

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

(define (handle-completed [j : JSExpr] [streaming : (Option Streaming) #f])
  : (Values String Msg)
  (match j
    [(hash 'id (? string? id) 'output (list outputs ...) #:open)
     (define-values (reason content tools)
       (for/fold
        ([reason : (Option String) #f]
         [content : String ""]
         [tools : (Listof ToolCall) '()])
        ([j : JSExpr (in-list outputs)])
         (match j
           [(hash 'content j #:open)
            (cond
              [(string? j) (values reason (string-append content j) tools)]
              [(list? j)
               (for/fold ([reason : (Option String) reason]
                          [content : String content]
                          [tools : (Listof ToolCall) tools])
                         ([j (in-list j)])
                 (match j
                   [(hash 'type "reasoning_text" 'text (? string? text) #:open)
                    (values (if reason (string-append reason text) text)
                            content
                            tools)]
                   [(hash 'type "output_text" 'text (? string? text) #:open)
                    (values reason (string-append content text) tools)]))]
              [else
               (error 'handle-completed "invalid response: ~a" j)])]
           [(hash 'type "function_call" 'call_id (? string? id) 'name (? string? name) 'arguments (? string? arguments) #:open)
            (values reason
                    content
                    (cons (ToolCall name arguments id) tools))]
           [_ (values reason content tools)])))
     (when streaming
       (when reason (streaming reason 'think))
       (streaming content 'content))
     (values id (Msg "assistant" content (reverse tools) #f reason))]))

(define ((handle-output [streaming : Streaming] [finish : (String Msg -> Void)]) [j : JSExpr])
  (match j
    [(hash 'type "response.completed" 'response response #:open)
     (let-values ([(id msg) (handle-completed response)])
       (finish id msg))]
    [(hash 'type "response.output_text.delta" 'delta (? string? text) #:open)
     (streaming text 'content)]
    [(hash 'type "response.reasoning_text.delta" 'delta (? string? text) #:open)
     (streaming text 'think)]
    [else (void)]))

(define response-ids : (HashTable Any String) (make-weak-hasheq))

(define (chat
         [i : Interactive] [streaming : Streaming] [opt : Options])
  (define data (jsexpr->bytes (build-oai-response-request i (hash-ref response-ids (current-history) (λ () #f)) opt)))
  ((current-network-trace) 'send data)
  (define-values (status headers body)
    (http-sendrecv/url (string->url (cast (Options-endpoint opt) String))
                       #:method "POST"
                       #:headers (cons "Content-Type: application/json" (Options-headers opt))
                       #:data data))
  (unless (string-contains? (bytes->string/latin-1 status) "200")
    (error 'send "status: ~a" status))
  (define-values (id msg)
    (begin0
      (if (parse-content-type-streaming? headers)
          (let/ec k : (Values String Msg)
            (on-event-stream body (handle-output streaming k))
            (error 'chat "incomplete"))
          (handle-completed (bytes->jsexpr (port->bytes body)) streaming))
      (close-input-port body)))
  (current-history (append (current-history) (->msgs i) (list msg)))
  (hash-set! response-ids (current-history) id)
  msg)