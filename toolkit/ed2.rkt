#lang racket
;; ed2.rkt --- history editor with tool call editing support.
;;
;; Based on ed.rkt, additionally supports:
;;  * editing tool calls (name / arguments / id) of assistant messages
;;  * adding / deleting / duplicating / reordering tool calls
;;  * pretty-print / minify / live json validation for tool call arguments
;;  * editing the tool-call-id of tool messages
;;
;; Tool calls are edited through mutable "drafts" (see `tc-draft'), which are
;; created lazily from `Msg-tool-calls' and cached in the per-entry table.
;; Drafts are the source of truth for the tool call tab, and are written back
;; into the message whenever the history is refreshed or editing is done.
;;
;; Usage: (require "ed2.rkt") (edit-history (current-history))
;;    or: racket toolkit/ed2.rkt     ;; standalone demo with fake tool calls
(require racket/gui/base
         racket/gui/easy
         racket/gui/easy/operator
         racket/string
         json
         rkt-llm-cli
         framework)

(provide edit-history input-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; message content <-> text%

(define (get-content text)
  (let loop ([s (send text find-first-snip)]
             [out (open-output-string)])
    (define (flush c)
      (define str (get-output-string out))
      (cond
        [(null? c) str]
        [(= 0 (string-length str)) c]
        [else (cons str c)]))
    (cond
      [(not s) (flush '())]
      [(is-a? s image-snip%)
       (define bm (send s get-bitmap))
       (define b (open-output-bytes))
       (send bm save-file b 'jpeg)
       (flush (cons (Image (get-output-bytes b))
                    (loop (send s next)
                          (open-output-string))))]
      [else (write-string (send s get-text 0 (send s get-count) #t) out)
            (loop (send s next) out)])))

(define (load-content ed content)
  (cond
    [(string? content) (send ed insert content)]
    [else
     (for ([c (in-list content)])
       (cond
         [(string? c) (send ed insert c (send ed last-position))]
         [(Image? c) (send ed insert
                           (make-object image-snip% (open-input-bytes (Image-data c)))
                           (send ed last-position))]))]))

(define (text->string ed)
  (send ed get-text))

(define (text-replace! ed str)
  (send ed begin-edit-sequence)
  (send ed select-all)
  (send ed insert str)
  (send ed end-edit-sequence)
  (send ed set-position 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editors

(define (make-editor-text content [on-change void])
  ;; a text% which calls `(on-change ed)' after user modification
  (define ready? #f)
  (define ed
    (new (class text%
           (init-field [change-handler void])
           (super-new)
           (define/augment (after-insert start len) (change-handler this))
           (define/augment (after-delete start len) (change-handler this)))
         [auto-wrap #t]
         [change-handler (λ (e) (when ready? (on-change e)))]))
  (send ed set-keymap (keymap:get-global))
  (send ed set-max-undo-history 100)
  (load-content ed content)
  (send ed set-modified #f)
  (set! ready? #t)
  ed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json helpers

(define (one-line str [cap 120])
  (define line (regexp-replace* #rx"[\r\n]+" str " "))
  (if (< (string-length line) cap)
      line
      (string-append (substring line 0 cap) "...")))

(define (blank? str)
  (for/and ([c (in-string str)])
    (char-whitespace? c)))

(define (json-read str)
  ;; reads exactly one json value, which must consume the whole string
  ;; (string->jsexpr alone stops at the end of the first value and silently
  ;;  ignores trailing garbage)
  ;; -> (values jsexpr #f) | (values #f error-message)
  (with-handlers ([exn:fail? (λ (e) (values #f (one-line (exn-message e))))])
    (define in (open-input-string str))
    (define v (read-json in))
    (define rest (port->string in))
    (cond
      [(eof-object? v) (values #f "empty json")]
      [(not (blank? rest))
       (values #f (format "trailing text after json: ~a" (one-line rest 40)))]
      [else (values v #f)])))

(define (json-check str)
  ;; -> (or/c #f string?), #f for valid json, the error message otherwise
  (cond
    [(blank? str) #f]
    [else (let-values ([(_ err) (json-read str)]) err)]))

(define (json-format str indent)
  ;; -> (or/c #f string?), formatted json or #f when not valid / empty
  (cond
    [(blank? str) #f]
    [else
     (let-values ([(v err) (json-read str)])
       (and (not err) (jsexpr->string v #:indent indent)))]))

(define (validate-throttled! @status)
  ;; validate outside of the current event, so that typing stays responsive
  (define pending? #f)
  (λ (ed)
    (unless pending?
      (set! pending? #t)
      (queue-callback
       (λ ()
          (set! pending? #f)
          (obs-set! @status (or (json-check (text->string ed)) "")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tool call drafts

;; NB: this type must stay non-transparent and non-prefab, so that `equal?' on
;; drafts is identity based -- `list-view' uses drafts themselves as view keys
(struct tc-draft (name id args status)
  ;; name   : obs?(string?)  tool call name
  ;; id     : obs?(string?)  tool call id
  ;; args   : text%          tool call arguments (raw json text)
  ;; status : obs?(string?)  validation message
  )

(define (fresh-call-id)
  (format "call_~a" (random 1000000)))

(define (make-tc-draft [tc (ToolCall "" "" "")])
  (define @status (obs ""))
  (tc-draft (obs (ToolCall-name tc))
            (obs (ToolCall-id tc))
            (make-editor-text (ToolCall-arguments tc) (validate-throttled! @status))
            @status))

(define (draft->tc d)
  (ToolCall (obs-peek (tc-draft-name d))
            (text->string (tc-draft-args d))
            (obs-peek (tc-draft-id d))))

(define (draft-index tcs d)
  (for/first ([x (in-list tcs)]
              [i (in-naturals)]
              #:when (eq? x d))
    i))

(define (list-move lst i j)
  (define e (list-ref lst i))
  (define wo (append (take lst i) (drop lst (add1 i))))
  (define j* (min j (length wo)))
  (append (take wo j*) (list e) (drop wo j*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-history history)
  (define/obs @history
    (for/vector #:length (length history) ([msg (in-list history)])
      (cons msg (make-hasheq))))

  (define/obs @new-role "user")
  (define/obs @idx (if (null? history) #f (sub1 (length history))))
  ;; bumped whenever the tool call list of the selected message changes
  (define/obs @rev 0)

  (define @idx-entry (obs-combine
                      (λ (h i) (and i (vector-ref h i)))
                      @history @idx))

  (define @idx-role (obs-map
                     @idx-entry
                     (λ (e) (and e (Msg-role (car e))))))

  ;; text% cache for content / reasoning, keyed by 'content / 'reason
  (define (get-text! content h key)
    (hash-ref! h key (λ () (make-editor-text content))))

  ;; obs?(string?) cache for the tool-call-id of a tool message, keyed by 'tcid
  (define (get-id-obs! entry)
    (hash-ref! (cdr entry) 'tcid
               (λ () (obs (or (Msg-tool-call-id (car entry)) "")))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; tool call drafts of the selected message, keyed by 'tcs

  (define (entry-tcs entry)
    (hash-ref! (cdr entry) 'tcs
               (λ ()
                 (list->vector
                  (for/list ([tc (in-list (Msg-tool-calls (car entry)))])
                    (make-tc-draft tc))))))

  (define @tcs
    (obs-combine
     (λ (h i _)
       (if i (vector->list (entry-tcs (vector-ref h i))) '()))
     @history @idx @rev))

  (define (update-tcs! f)
    (define idx (obs-peek @idx))
    (when idx
      (define entry (vector-ref (obs-peek @history) idx))
      (hash-set! (cdr entry) 'tcs
                 (list->vector (f (vector->list (entry-tcs entry)))))
      (obs-update! @rev add1)
      (sync-history!)))

  (define ((add-tc!))
    (update-tcs! (λ (tcs) (append tcs (list (make-tc-draft))))))

  (define (delete-tc! d)
    (update-tcs! (λ (tcs) (filter (λ (x) (not (eq? x d))) tcs))))

  (define (copy-tc! d)
    (update-tcs!
     (λ (tcs)
       (define i (or (draft-index tcs d) (sub1 (length tcs))))
       (define new
         (make-tc-draft
          (ToolCall (obs-peek (tc-draft-name d))
                    (text->string (tc-draft-args d))
                    (fresh-call-id))))
       (append (take tcs (add1 i)) (list new) (drop tcs (add1 i))))))

  (define (move-tc! d delta)
    (update-tcs!
     (λ (tcs)
       (define i (draft-index tcs d))
       (define j (and i (+ i delta)))
       (if (and j (<= 0 j (sub1 (length tcs))))
           (list-move tcs i j)
           tcs))))

  (define (reformat-args! d indent)
    (define ed (tc-draft-args d))
    (define str (text->string ed))
    (define formatted (json-format str indent))
    (cond
      [(and (not formatted) (not (blank? str)))
       (obs-set! (tc-draft-status d) (or (json-check str) "invalid json"))]
      [formatted
       (text-replace! ed formatted)
       (obs-set! (tc-draft-status d) "")]
      [else
       (obs-set! (tc-draft-status d) "")]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; write back

  ;; flush a text% cache entry when it was edited
  (define (flush-edited! h msg key update)
    (cond
      [(hash-ref h key (λ () #f))
       =>
       (λ (ed)
         (cond
           [(send ed is-modified?)
            (send ed set-modified #f)
            (update msg (get-content ed))]
           [else msg]))]
      [else msg]))

  ;; an empty tool-call-id text means "no id"
  (define (normalize-id v)
    (if (and (string? v) (string=? v "")) #f v))

  (define (entry->msg entry)
    (define h (cdr entry))
    (define msg1
      (flush-edited! h (car entry) 'content
                     (λ (m c) (struct-copy Msg m [content c]))))
    (define msg2
      (flush-edited! h msg1 'reason
                     (λ (m c) (struct-copy Msg m [reasoning-content c]))))
    (define msg3
      (cond
        [(hash-ref h 'tcid (λ () #f))
         =>
         (λ (@id)
           (define id (normalize-id (obs-peek @id)))
           (if (equal? id (Msg-tool-call-id msg2))
               msg2
               (struct-copy Msg msg2 [tool-call-id id])))]
        [else msg2]))
    ;; tool calls always come from the drafts once they are created
    (cond
      [(hash-ref h 'tcs (λ () #f))
       =>
       (λ (tcs)
         (define new-tcs (map draft->tc (vector->list tcs)))
         (if (equal? new-tcs (Msg-tool-calls msg3))
             msg3
             (struct-copy Msg msg3 [tool-calls new-tcs])))]
      [else msg3]))

  (define (sync-history!)
    (obs-update!
     @history
     (λ (history)
       (for/vector #:length (vector-length history) ([entry (in-vector history)])
         (cons (entry->msg entry) (cdr entry))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; message list

  (define (entry->row entry)
    (define (->label str)
      (define cap 20)
      (if (< (string-length str) cap) str (string-append (substring str 0 cap) "...")))
    (define msg (car entry))
    (define tcs (hash-ref (cdr entry) 'tcs (λ () #f)))
    (vector (Msg-role msg)
            (->label (cond
                       [(string? (Msg-content msg)) (Msg-content msg)]
                       [else (string-join (filter string? (Msg-content msg)))]))
            (number->string (if tcs
                                (vector-length tcs)
                                (length (Msg-tool-calls msg))))))

  (define (select-entry! idx)
    (:= @idx idx)
    (:= @tab (obs-peek @default-tab))
    (sync-history!))

  (define ((insert-one! f))
    (define history (obs-peek @history))
    (define idx (obs-peek @idx))
    (define role (obs-peek @new-role))
    (define (new-one)
      (cons
       (case role
         [("user") (make-user "")]
         [("assistant") (make-assistant "")]
         [else (error 'insert-one!)])
       (make-hasheq)))
    (cond
      [(and idx role)
       (define pos (f idx))
       (define new-entries
         (build-vector
          (add1 (vector-length history))
          (λ (i)
            (cond
              [(< i pos) (vector-ref history i)]
              [(= i pos) (new-one)]
              [else (vector-ref history (sub1 i))]))))
       (:= @history new-entries)
       (:= @idx pos)
       (:= @tab "content")
       (obs-update! @rev add1)]
      [(vector-empty? history)
       (:= @history (vector (new-one)))
       (:= @idx 0)]))

  (define (delete-one!)
    (define history (obs-peek @history))
    (define idx (obs-peek @idx))
    (define role (obs-peek @new-role))
    (when (and idx role)
      (define new-entries
        (build-vector
         (sub1 (vector-length history))
         (λ (i)
           (cond
             [(< i idx) (vector-ref history i)]
             [else (vector-ref history (add1 i))]))))
      (:= @idx #f)
      (:= @history new-entries)
      (obs-update! @rev add1)))

  (define @default-tab
    (obs-combine
     (λ (history idx)
       (cond
         [(not idx) "content"]
         [else
          (define msg (car (vector-ref history idx)))
          (if (and (string=? (Msg-role msg) "assistant")
                   (Msg-tool-calls msg)
                   (not (null? (Msg-tool-calls msg))))
              "tool-call"
              "content")]))
     @history @idx))

  (define/obs @tab (obs-peek @default-tab))
  (define/obs @visible? #t)
  (define done? #f)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; views

  (define (tool-call-view d @entry)
    ;; `list-view' passes the entry itself as the first argument (identity based
    ;; key, see `tc-draft'), the second one is an obs re-pointing to that entry.
    ;; Drafts are stable objects, so the plain value is all we need.
    (define @name (tc-draft-name d))
    (define @id (tc-draft-id d))
    (define @status (tc-draft-status d))
    ;; always show a status label, so that an empty one does not collapse the row
    (define @status-label
      (obs-map @status (λ (s) (if (string=? s "") "json ok" s))))
    (define @status-color
      (obs-map @status (λ (s) (if (string=? s "") "gray" "red"))))
    (vpanel
     #:style '(border)
     (hpanel
      #:stretch '(#t #f)
      (text (obs-map @tcs (λ (tcs) (format "call ~a" (add1 (or (draft-index tcs d) 0)))))
            #:stretch '(#f #f)
            #:margin '(4 4))
      (input @name
             (λ (evt str) (when (eq? evt 'input) (obs-set! @name str)))
             #:label "name")
      (input @id
             (λ (evt str) (when (eq? evt 'input) (obs-set! @id str)))
             #:label "id"
             #:stretch '(#f #f)
             #:min-size '(180 #f)))
     (text "arguments (json)")
     (editor-canvas (tc-draft-args d)
                    #:style '(auto-vscroll)
                    #:min-size '(#f 100)
                    #:stretch '(#t #f))
     (hpanel
      #:stretch '(#t #f)
      (text @status-label #:color @status-color #:stretch '(#t #f) #:margin '(8 4))
      (button "Format" (λ () (reformat-args! d 2)))
      (button "Minify" (λ () (reformat-args! d #f)))
      (button "Up" (λ () (move-tc! d -1)))
      (button "Down" (λ () (move-tc! d 1)))
      (button "Duplicate" (λ () (copy-tc! d)))
      (button "Delete" (λ () (delete-tc! d))))))

  (define (content-view)
    (editor-canvas
     (obs-map @idx-entry
              (λ (e) (if e
                         (get-text! (Msg-content (car e)) (cdr e) 'content)
                         #f)))))

  ;; current tool-call-id of a tool message, kept in an obs so that it can be
  ;; edited with a plain text field
  (define @tool-call-id
    (obs-map @idx-entry
             (λ (e) (if e (obs-peek (get-id-obs! e)) ""))))

  (define (update-tool-call-id! str)
    (define e (obs-peek @idx-entry))
    (when e (obs-set! (get-id-obs! e) str)))

  (render
   (window
    #:min-size '(800 600)
    #:mixin (λ (%)
              (class %
                (super-new)
                (obs-observe!
                 @visible?
                 (λ (visible?)
                   (send this show visible?)
                   (send this on-close)))))
    (vpanel
     (hpanel
      (vpanel
       #:stretch '(#f #t)
       (table '("role" "content" "calls")
              @history
              (λ (type entries idx)
                (when (and (eq? type 'select) idx)
                  (select-entry! idx)))
              #:entry->row entry->row
              #:selection @idx
              #:style '(single column-headers))
       (hpanel
        #:stretch '(#t #f)
        (choice '("user" "assistant")
                (λ (role) (@new-role . := . role)))
        (vpanel
         (button "Insert before" (insert-one! values))
         (button "Insert After" (insert-one! add1))
         (button "Delete" delete-one!))))
      (vpanel
       (case-view
        @idx-role
        [("assistant")
         (tabs '("content" "reason" "tool-call")
               (λ (type _ sel)
                 (when (eq? type 'select)
                   (:= @tab sel)))
               (case-view
                @tab
                [("content")
                 (content-view)]
                [("reason")
                 (editor-canvas (obs-map
                                 @idx-entry
                                 (λ (e) (if e
                                            (get-text! (or (Msg-reasoning-content (car e)) "") (cdr e) 'reason)
                                            #f))))]
                [("tool-call")
                 (vpanel
                  (list-view @tcs
                             tool-call-view
                             #:spacing 4
                             #:style '(vertical auto-vscroll))
                  (hpanel
                   #:stretch '(#t #f)
                   (button "Add tool call" (add-tc!))
                   (text (obs-map @tcs (λ (tcs) (format "~a tool call(s)" (length tcs))))
                         #:color "gray"
                         #:margin '(8 4))))]
                [else (content-view)])
               #:selection @tab)]
        [("tool")
         (vpanel
          (text "content")
          (content-view)
          (input @tool-call-id
                 (λ (evt str) (when (eq? evt 'input) (update-tool-call-id! str)))
                 #:label "tool-call-id"
                 #:stretch '(#t #f)))]
        [else
         (vpanel
          (text "content")
          (content-view))])))
     (hpanel
      #:stretch '(#t #f) #:alignment '(center center)
      (button "Done" (λ () (set! done? #t) (:= @visible? #f)))
      (button "Cancel" (λ () (:= @visible? #f))))))
   #:wait? #t)

  (cond
    [(not done?) #f]
    [else
     (sync-history!)
     (for/list ([entry (in-vector (obs-peek @history))])
       (entry->msg entry))]))

(define (input-history)
  (cond
    [(edit-history (current-history))
     => current-history]))

(module+ main
  (define t
    (list (make-user "list files in /tmp")
          (struct-copy Msg (make-assistant "Let me list them.")
                       [tool-calls
                        (list (ToolCall "shell_exec"
                                        (jsexpr->string (hasheq 'cmd "ls /tmp | head -3"))
                                        "call_1")
                              (ToolCall "shell_exec" "{}" "call_2"))])
          (make-tool "/tmp is empty\n" "call_1")))
  (define new-history (edit-history t))
  (when new-history
    (for ([msg (in-list new-history)])
      (printf "~a\n" msg))))

(module+ test
  (require rackunit)
  (check-false (json-check ""))
  (check-false (json-check "{\"a\": 1}"))
  (check-false (json-check "  {\"a\": 1}  ")) ; trailing whitespace is fine
  (check-false (json-check "123"))
  (check-not-false (string? (json-check "{\"a\":")))
  ;; string->jsexpr would accept these, json-check must not
  (check-not-false (string? (json-check "{\"a\":1} garbage")))
  (check-not-false (string? (json-check "{\"a\":1} {bad")))
  (check-equal? (json-format "{\"a\":1}" #f) "{\"a\":1}")
  (check-not-false (string? (json-format "{\"a\":1}" 2)))
  (check-false (json-format "{" 2))
  (check-false (json-format "{\"a\":1} garbage" 2)) ; never drop trailing text
  (check-equal? (list-move '(a b c d) 1 2) '(a c b d))
  (check-equal? (list-move '(a b c d) 2 1) '(a c b d))
  (check-equal? (list-move '(a b c d) 3 0) '(d a b c))
  ;; drafts
  (define d1 (make-tc-draft (ToolCall "a" "" "1")))
  (define d2 (make-tc-draft (ToolCall "b" "{}" "2")))
  (define d3 (make-tc-draft (ToolCall "c" "{}" "3")))
  (define ds (list d1 d2 d3))
  (check-eq? (draft-index ds d2) 1)
  (check-false (draft-index ds (make-tc-draft)))
  (check-eq? (list-ref (list-move ds 1 2) 2) d2) ; move down keeps identity
  (check-eq? (list-ref (list-move ds 1 0) 0) d2) ; move up keeps identity
  (check-eq? (list-ref (append (take ds 2) (list d2) (drop ds 2)) 2) d2) ; duplicate
  (send (tc-draft-args d1) insert "{\"x\": 1}")
  (obs-set! (tc-draft-name d1) "exec")
  (check-equal? (draft->tc d1) (ToolCall "exec" "{\"x\": 1}" "1"))
  (check-equal? (draft->tc d2) (ToolCall "b" "{}" "2"))
  (check-equal? (map (λ (d) (obs-peek (tc-draft-name d))) ds) '("exec" "b" "c"))
  ;; validation is wired to the args editor, and runs in a queued callback
  (define dv (make-tc-draft (ToolCall "t" "{\"a\":1}" "1")))
  (check-equal? (obs-peek (tc-draft-status dv)) "")
  (send (tc-draft-args dv) insert " garbage")
  (sleep/yield 0.5)
  (check-not-false (string? (obs-peek (tc-draft-status dv))))
  (text-replace! (tc-draft-args dv) "{\"a\":1}")
  (sleep/yield 0.5)
  (check-equal? (obs-peek (tc-draft-status dv)) ""))
