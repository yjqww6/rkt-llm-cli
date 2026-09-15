#lang racket

;; =====================================================================
;;  mcp-client  --  A Model Context Protocol (MCP) client for Racket.
;;
;;  This module reimplements the *client side* of `bridge.py` as a pure
;;  Racket function interface.  Instead of exposing HTTP endpoints
;;  (`GET /tools`, `POST /tool_call`) it exposes plain Racket functions:
;;
;;      (make-mcp-client server-configs)   ; connect + initialize servers
;;      (get-tools client)                 ;  -> (listof openai-tool)
;;      (call-tool client name arguments)  ;  -> call-tool-result
;;      (close-mcp-client client)          ; shut everything down
;;
;;  Two wire transports are supported, matching bridge.py:
;;
;;    * stdio            (newline-delimited JSON-RPC 2.0 over a subprocess)
;;    * streamable HTTP  (JSON-RPC 2.0 over HTTP POST / SSE)
;;
;;  Both are hidden behind a common session interface so that
;;  `mcp-request` / `mcp-notify` / `close-mcp-session` work for either.
;; =====================================================================

(require json
         racket/system
         racket/port
         racket/path
         racket/match
         racket/string
         net/http-client
         net/url)

(provide
 ;; config
 (struct-out mcp-stdio-server-config)
 (struct-out mcp-http-server-config)

 ;; client
 make-mcp-client
 mcp-client-get-tools
 mcp-client-call-tool
 close-mcp-client)

;; ---------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------

;; A stdio server: command plus its argument list.
(struct mcp-stdio-server-config (command args) #:transparent)

;; A streamable-HTTP server: the MCP endpoint URL.
(struct mcp-http-server-config (url) #:transparent)

;; Protocol version we announce during the initialize handshake.
(define latest-protocol-version "2025-11-25")

;; ---------------------------------------------------------------------
;; Session structures
;; ---------------------------------------------------------------------

;; stdio: `stdout` is an input-port we read JSON-RPC responses from,
;; `stdin`  is an output-port we write JSON-RPC requests to,
;; `stderr` captures the server's stderr (diagnostics),
;; `control` is the `process*` control procedure,
;; `pending` maps in-flight request ids -> async channels,
;; `id-counter` is the next request id,
;; `reader` is the background response-dispatch thread.
(struct mcp-stdio-session (stdout stdin stderr control pending id-counter reader)
  #:mutable #:transparent)

;; streamable HTTP: `url` is a `url?` object, `session-id` is the
;; Mcp-Session-Id (or #f before initialize), `protocol-version` is the
;; negotiated Mcp-Protocol-Version (or #f), `id-counter` is the next id.
(struct mcp-http-session (url session-id protocol-version id-counter)
  #:mutable #:transparent)

;; ---------------------------------------------------------------------
;; Low-level stdio transport
;; ---------------------------------------------------------------------

;; Spawn `command` (plus `args`) and return a live stdio session.
;; Note: `process*` does not search PATH, so we resolve the executable
;; to a full path first.
(define (start-mcp-stdio-session cfg)
  (define command (mcp-stdio-server-config-command cfg))
  (define exe (find-executable-path command))
  (unless exe
    (error 'start-mcp-stdio-session "executable ~a not found on PATH" command))
  (define proc (apply process* (cons (path->string exe)
                                     (mcp-stdio-server-config-args cfg))))
  (match proc
    [(list stdout stdin pid stderr control)
     (define pending (make-hash))
     (define session
       (mcp-stdio-session stdout stdin stderr control pending 0 #f))
     (set-mcp-stdio-session-reader! session
                                    (thread (lambda ()
                                              (mcp-stdio-reader-loop session))))
     ;; Drain stderr on a background thread so the pipe never fills up
     ;; (which would block the server) and we can surface diagnostics.
     (thread (lambda () (mcp-stderr-loop session)))
     session]
    [_ (error 'start-mcp-stdio-session "unexpected process* result: ~a" proc)]))

;; Read and (optionally) log the server's stderr until EOF.  This keeps the
;; stderr pipe from filling and blocking the subprocess.
(define (mcp-stderr-loop session)
  (define err (mcp-stdio-session-stderr session))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (let loop ()
      (define line (read-line err))
      (unless (eof-object? line)
        (eprintf "mcp-client [server stderr]: ~a~n" line)
        (loop)))))

;; Read newline-delimited JSON-RPC messages from the server and route
;; responses to the waiting request channels.
(define (mcp-stdio-reader-loop session)
  (define in (mcp-stdio-session-stdout session))
  ;; If the stream ends (EOF) or reading raises (e.g. the port was closed),
  ;; fail every in-flight request so callers don't hang forever.
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (mcp-stdio-fail-all-pending session))])
    (let loop ()
      (define line (read-line in))
      (if (eof-object? line)
          (mcp-stdio-fail-all-pending session)
          (begin
            (mcp-stdio-handle-line session line)
            (loop))))))

;; Signal `mcp-response-eof` to every pending request.
(define (mcp-stdio-fail-all-pending session)
  (for ([id (hash-keys (mcp-stdio-session-pending session))])
    (define ch (hash-ref (mcp-stdio-session-pending session) id))
    (channel-put ch (mcp-response-eof))))

;; Parse and dispatch a single JSON-RPC line.
(define (mcp-stdio-handle-line session line)
  (when (string? line)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (eprintf "mcp-client: ignoring bad JSON line: ~a~n" line))])
      (define msg (string->jsexpr line))
      ;; A response carries an `id` that matches a pending request.
      (when (hash-has-key? msg 'id)
        (define id (hash-ref msg 'id))
        (define ch (hash-ref (mcp-stdio-session-pending session) id #f))
        (when ch
          (channel-put ch msg))))))

;; A sentinel used to signal that the server went away.
(define (mcp-response-eof) (quote mcp-response-eof))

;; Serialise a jsexpr as a single newline-terminated JSON line.
(define (send-json out obj)
  (write-string (jsexpr->string obj) out)
  (newline out)
  (flush-output out))

;; Allocate the next request id for a stdio session (monotonic).
(define (mcp-stdio-next-id session)
  (set-mcp-stdio-session-id-counter! session (add1 (mcp-stdio-session-id-counter session)))
  (mcp-stdio-session-id-counter session))

;; Send a JSON-RPC request over stdio and wait for its result.
(define (mcp-stdio-request session method params)
  (define id (mcp-stdio-next-id session))
  (define ch (make-channel))
  (hash-set! (mcp-stdio-session-pending session) id ch)
  (send-json (mcp-stdio-session-stdin session)
             (hasheq 'jsonrpc "2.0" 'id id 'method method 'params params))
  (define resp (channel-get ch))
  (hash-remove! (mcp-stdio-session-pending session) id)
  (cond
    [(equal? resp (mcp-response-eof))
     (error 'mcp-stdio-request "MCP server closed connection while waiting for ~a" method)]
    [(hash-has-key? resp 'error)
     (define err (hash-ref resp 'error))
     (error 'mcp-stdio-request
            (format "MCP ~a failed: ~a (code ~a)"
                    method
                    (or (hash-ref err 'message #f) "unknown error")
                    (hash-ref err 'code #f)))]
    [else
     (hash-ref resp 'result)]))

;; Send a JSON-RPC notification over stdio (no response expected).
(define (mcp-stdio-notify session method params)
  (send-json (mcp-stdio-session-stdin session)
             (hasheq 'jsonrpc "2.0" 'method method 'params params)))

;; Close the stdio session: signal EOF to the server, wait for it to exit,
;; then close the remaining pipes so nothing leaks.
(define (mcp-stdio-close session)
  (define stdin (mcp-stdio-session-stdin session))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (close-output-port stdin))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    ((mcp-stdio-session-control session) 'wait))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (close-input-port (mcp-stdio-session-stdout session)))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (close-input-port (mcp-stdio-session-stderr session))))

;; ---------------------------------------------------------------------
;; Low-level streamable-HTTP transport
;; ---------------------------------------------------------------------

;; Parse the HTTP status line (e.g. #"HTTP/1.1 200 OK") into a number.
(define (mcp-parse-status status-bytes)
  (define s (bytes->string/utf-8 status-bytes))
  (define parts (string-split s " "))
  (if (>= (length parts) 2)
      (or (string->number (cadr parts)) 0)
      0))

;; Find the value of a header (case-insensitive) from a list of header
;; byte strings returned by http-sendrecv/url.
(define (mcp-find-header headers name)
  (for/or ([h (in-list headers)])
    (define s (bytes->string/utf-8 h))
    (define colon (string-find s ":"))
    (and colon
         (let* ([hname (string-trim (substring s 0 colon))]
                [hval (string-trim (substring s (add1 colon)))])
           (and (string-ci=? hname name) hval)))))

;; Read a JSON-RPC message from an SSE stream.  `event: message` events
;; carry the message in their `data:` lines; notifications (e.g. progress)
;; are ignored until a response/error (with an `id`) arrives.
(define (mcp-http-read-sse body-port)
  (let loop ()
    (define line (read-line body-port))
    (cond
      [(eof-object? line)
       (error 'mcp-http-read-sse "SSE stream ended before a response")]
      [(string-prefix? line "data: ")
       (define json-str (substring line 6))
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (eprintf "mcp-client: ignoring bad SSE data line: ~a~n" line)
                          (loop))])
         (define msg (string->jsexpr json-str))
         (cond
           [(and (hash-has-key? msg 'id)
                 (or (hash-has-key? msg 'result)
                     (hash-has-key? msg 'error)))
            msg]
           [else (loop)]))]
      [else (loop)])))

;; Allocate the next request id for an HTTP session (monotonic).
(define (mcp-http-next-id session)
  (set-mcp-http-session-id-counter! session (add1 (mcp-http-session-id-counter session)))
  (mcp-http-session-id-counter session))

;; POST a JSON-RPC message to the HTTP endpoint and parse the response.
;; Returns (values status-code headers msg) where `msg` is the parsed
;; JSON-RPC message (or #f for a 202 Accepted / empty body).
(define (mcp-http-post session msg)
  (define u (mcp-http-session-url session))
  (define headers
    (list "Accept: application/json, text/event-stream"
          "Content-Type: application/json"
          (format "Mcp-Protocol-Version: ~a"
                  (or (mcp-http-session-protocol-version session)
                      latest-protocol-version))))
  (define headers*
    (if (mcp-http-session-session-id session)
        (append headers
                (list (format "Mcp-Session-Id: ~a"
                              (mcp-http-session-session-id session))))
        headers))
  (define body (jsexpr->string msg))
  (define-values (status-bytes resp-headers body-port)
    (http-sendrecv/url u
                       #:method "POST"
                       #:headers headers*
                       #:data body))
  (define status-code (mcp-parse-status status-bytes))
  (define msg*
    (cond
      [(equal? status-code 202) #f]
      [else
       (define ct (mcp-find-header resp-headers "Content-Type"))
       (cond
         [(and ct (string-prefix? ct "text/event-stream"))
          (mcp-http-read-sse body-port)]
         [else
          (define body-str (port->string body-port))
          (if (string=? (string-trim body-str) "")
              #f
              (string->jsexpr body-str))])]))
  (close-input-port body-port)
  (values status-code resp-headers msg*))

;; Send a JSON-RPC request over HTTP and wait for its result.
(define (mcp-http-request session method params)
  (define id (mcp-http-next-id session))
  (define-values (status headers msg)
    (mcp-http-post session
                   (hasheq 'jsonrpc "2.0" 'id id 'method method 'params params)))
  (unless msg
    (error 'mcp-http-request "MCP ~a got HTTP ~a (no body)" method status))
  (cond
    [(hash-has-key? msg 'error)
     (define err (hash-ref msg 'error))
     (error 'mcp-http-request
            (format "MCP ~a failed: ~a (code ~a)"
                    method
                    (or (hash-ref err 'message #f) "unknown error")
                    (hash-ref err 'code #f)))]
    [else
     (hash-ref msg 'result)]))

;; Send a JSON-RPC notification over HTTP (no response expected).
(define (mcp-http-notify session method params)
  (define-values (status headers msg)
    (mcp-http-post session
                   (hasheq 'jsonrpc "2.0" 'method method 'params params)))
  (void))

;; Close an HTTP session.  There is no persistent connection to tear down
;; (http-sendrecv/url opens a fresh connection per request), so we just
;; forget the session id.
(define (mcp-http-close session)
  (set-mcp-http-session-session-id! session #f)
  (void))

;; Create a fresh HTTP session from its config.  The actual handshake is
;; performed by `mcp-http-initialize`.
(define (start-mcp-http-session cfg)
  (define u (string->url (mcp-http-server-config-url cfg)))
  (mcp-http-session u #f #f 0))

;; Perform the initialize handshake over HTTP, capturing the session id
;; from the response headers and the negotiated protocol version from the
;; result.  Then send `notifications/initialized`.
(define (mcp-http-initialize session)
  (define id (mcp-http-next-id session))
  (define-values (status headers msg)
    (mcp-http-post session
                   (hasheq 'jsonrpc "2.0" 'id id 'method "initialize"
                           'params (hasheq 'protocolVersion latest-protocol-version
                                           'capabilities (hasheq)
                                           'clientInfo (hasheq 'name "mcp-client"
                                                               'version "0.1.0")))))
  (unless (and msg (hash-has-key? msg 'result))
    (error 'mcp-http-initialize "initialize failed (HTTP ~a)" status))
  (define sid (mcp-find-header headers "Mcp-Session-Id"))
  (when sid (set-mcp-http-session-session-id! session sid))
  (define result (hash-ref msg 'result))
  (define pv (hash-ref result 'protocolVersion #f))
  (when pv (set-mcp-http-session-protocol-version! session pv))
  (mcp-notify session "notifications/initialized" (hasheq))
  result)

;; ---------------------------------------------------------------------
;; Generic session dispatchers
;; ---------------------------------------------------------------------

;; Send a JSON-RPC request and wait for its result.
(define (mcp-request session method params)
  (cond
    [(mcp-stdio-session? session) (mcp-stdio-request session method params)]
    [(mcp-http-session? session) (mcp-http-request session method params)]
    [else (error 'mcp-request "unknown session type: ~a" session)]))

;; Send a JSON-RPC notification (no response expected).
(define (mcp-notify session method params)
  (cond
    [(mcp-stdio-session? session) (mcp-stdio-notify session method params)]
    [(mcp-http-session? session) (mcp-http-notify session method params)]
    [else (error 'mcp-notify "unknown session type: ~a" session)]))

;; Close the session (stdio: signal EOF + wait; HTTP: clear session id).
(define (close-mcp-session session)
  (cond
    [(mcp-stdio-session? session) (mcp-stdio-close session)]
    [(mcp-http-session? session) (mcp-http-close session)]
    [else (error 'close-mcp-session "unknown session type: ~a" session)]))

;; ---------------------------------------------------------------------
;; MCP high-level operations
;; ---------------------------------------------------------------------

;; Perform the initialize handshake (transport-aware).
(define (mcp-initialize session)
  (cond
    [(mcp-stdio-session? session)
     (mcp-stdio-request session "initialize"
                        (hasheq 'protocolVersion latest-protocol-version
                                'capabilities (hasheq)
                                'clientInfo (hasheq 'name "mcp-client"
                                                    'version "0.1.0")))
     (mcp-stdio-notify session "notifications/initialized" (hasheq))]
    [(mcp-http-session? session)
     (mcp-http-initialize session)]
    [else (error 'mcp-initialize "unknown session type: ~a" session)]))

;; List the server's tools.
(define (mcp-list-tools session)
  (define result (mcp-request session "tools/list" (hasheq)))
  (hash-ref result 'tools))

;; Call a server tool.
(define (mcp-call-tool session name arguments)
  (mcp-request session "tools/call"
               (hasheq 'name name 'arguments arguments)))

;; ---------------------------------------------------------------------
;; Client aggregate
;; ---------------------------------------------------------------------

;; A client owns several sessions, a tool-name -> session index, the
;; cached OpenAI tools, and a dedicated custodian that owns all of the
;; client's resources (subprocesses, threads, ports) so that a single
;; `close-mcp-client` tears everything down.
(struct mcp-client (sessions tool->session tools custodian) #:transparent)

;; Start a session from a config, dispatching on config type.
(define (start-mcp-session cfg)
  (cond
    [(mcp-stdio-server-config? cfg) (start-mcp-stdio-session cfg)]
    [(mcp-http-server-config? cfg) (start-mcp-http-session cfg)]
    [else (error 'start-mcp-session "unknown config: ~a" cfg)]))

;; Human-readable label for a config (used in warning messages).
(define (mcp-config-label cfg)
  (cond
    [(mcp-stdio-server-config? cfg) (mcp-stdio-server-config-command cfg)]
    [(mcp-http-server-config? cfg) (mcp-http-server-config-url cfg)]
    [else "?"]))

;; Connect to every server config, handshake, and cache the tools.
;; Servers that fail to start or initialize are skipped (with a warning),
;; matching bridge.py's behaviour.
;;
;; All resources (subprocesses, reader threads, ports) are created under a
;; dedicated custodian, so `close-mcp-client` can reclaim them in one shot.
(define (make-mcp-client server-configs)
  (define client-custodian (make-custodian))
  (define-values (sessions tool->session all-tools)
    (parameterize ([current-custodian client-custodian]
                   [current-subprocess-custodian-mode 'kill])
      (define sessions
        (for/list ([cfg (in-list server-configs)])
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (eprintf "mcp-client: skipping server ~a: ~a~n"
                                      (mcp-config-label cfg)
                                      (exn-message e))
                             #f)])
            (start-mcp-session cfg))))
      (define live (filter identity sessions))
      (define ok-sessions
        (for/list ([s (in-list live)])
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (eprintf "mcp-client: skipping session after initialize: ~a~n"
                                      (exn-message e))
                             (close-mcp-session s)
                             #f)])
            (mcp-initialize s)
            s)))
      (define tool->session (make-hash))
      (define all-tools '())
      (for ([s (in-list (filter identity ok-sessions))])
        (for ([tool (in-list (mcp-list-tools s))])
          (define name (hash-ref tool 'name))
          (when (hash-has-key? tool->session name)
            (eprintf "mcp-client: warning: tool ~a already exists; overriding.~n" name))
          (hash-set! tool->session name s)
          (set! all-tools (cons tool all-tools))))
      (values (filter identity ok-sessions) tool->session (reverse all-tools))))
  (mcp-client sessions tool->session
              (map convert-to-openai-tool all-tools)
              client-custodian))

;; Function interface replacing `GET /tools`.
(define (mcp-client-get-tools client)
  (mcp-client-tools client))

;; Function interface replacing `POST /tool_call`.
(define (mcp-client-call-tool client name arguments)
  (define s (hash-ref (mcp-client-tool->session client) name #f))
  (unless s
    (error 'call-tool "tool ~a not found" name))
  (mcp-call-tool s name arguments))

;; Shut down all sessions and reclaim every resource the client owns.
;; Shutting down the custodian first terminates the subprocesses (via the
;; 'kill subprocess-custodian-mode) and kills the reader/stderr threads, so
;; the explicit `close-mcp-session` calls below never block waiting on a
;; server that refuses to exit.
(define (close-mcp-client client)
  (custodian-shutdown-all (mcp-client-custodian client))
  (for ([s (in-list (mcp-client-sessions client))])
    (close-mcp-session s)))

;; ---------------------------------------------------------------------
;; MCP -> OpenAI tool conversion (mirrors bridge.py)
;; ---------------------------------------------------------------------

;; Recursively convert an MCP input schema into an OpenAI-style schema.
(define (convert-schema schema)
  (cond
    [(and (hash? schema) (hash-has-key? schema 'properties))
     (hasheq 'type "object"
             'properties
             (for/hash ([(k v) (in-hash (hash-ref schema 'properties))])
               (values k (convert-schema v)))
             'required (hash-ref schema 'required '()))]
    [(and (hash? schema) (or (hash-has-key? schema 'oneOf)
                             (hash-has-key? schema 'anyOf)))
     (define key (if (hash-has-key? schema 'oneOf) 'oneOf 'anyOf))
     (hasheq key (map convert-schema (hash-ref schema key)))]
    [(and (hash? schema) (equal? (hash-ref schema 'type #f) "array"))
     (hasheq 'type "array"
             'items (convert-schema (hash-ref schema 'items)))]
    [else schema]))

;; Convert a raw MCP tool definition to an OpenAI-style tool.
(define (convert-to-openai-tool tool)
  (define desc (hash-ref tool 'description ""))
  (define desc* (if (equal? desc (json-null)) "" desc))
  (hasheq 'name (hash-ref tool 'name)
          'description desc*
          'parameters (convert-schema (hash-ref tool 'inputSchema))))
