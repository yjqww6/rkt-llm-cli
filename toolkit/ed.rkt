#lang racket
(require racket/gui/base
         racket/gui/easy
         racket/gui/easy/operator
         rkt-llm-cli)

(provide edit-history input-history)

(define (get-content text)
  (let loop ([s (send text find-first-snip)]
             [out (open-output-string)])
    (define (flush c)
      (define str (get-output-string out))
      (cond
        [(= 0 (string-length str)) c]
        [(null? c) str]
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

(define (make-keymap)
  (define k (new keymap%))
  (add-editor-keymap-functions k)
  (add-text-keymap-functions k)
  ((current-text-keymap-initializer) k)
  k)

(define (edit-history history)
  (define/obs @history
    (for/vector #:length (length history) ([msg (in-list history)])
      (cons msg (make-hasheq))))

  (define/obs @new-role "user")
  (define/obs @idx (if (null? history) #f (sub1 (length history))))

  (define @idx-entry (obs-combine
                      (λ (h i) (and i (vector-ref h i)))
                      @history @idx))

  (define @idx-role (obs-map
                     @idx-entry
                     (λ (e) (and e (Msg-role (car e))))))

  (define k (make-keymap))

  (define (get-text! content h key)
    (hash-ref! h key
               (λ ()
                 (define t (new text%))
                 (send t set-keymap k)
                 (send t set-max-undo-history 100)
                 (load-content t content)
                 (send t set-modified #f)
                 t)))

  (define (entry->row entry)
    (define (content->label content)
      (define (->label str)
        (define cap 20)
        (if (< (string-length str) cap) str (string-append (substring str 0 cap) "...")))
      (cond
        [(string? content) (->label content)]
        [else
         (->label (string-join (filter string? content)))]))
    (define msg (car entry))
    (vector (Msg-role msg)
            (content->label (Msg-content msg))))

  (define (entry->msg entry)
    (define-syntax with
      (syntax-rules ()
        [(_ Id Expr) Expr]
        [(_ Id Expr Body0 Body ...)
         (with Id (let ([Id Expr])
                    Body0)
               Body ...)]))
    (define-syntax-rule (check-text msg t expr)
      (cond
        [(hash-ref (cdr entry) 't (λ () #f))
         =>
         (λ (t) (cond
                  [(send t is-modified?)
                   (send t set-modified #f)
                   expr]
                  [else msg]))]
        [else msg]))
    (with
     msg (car entry)
     (check-text msg content (struct-copy Msg msg [content (get-content content)]))
     (check-text msg reason (struct-copy Msg msg [reasoning-content (get-content reason)]))))

  (define (refresh-entry!)
    (obs-update!
     @history
     (λ (history)
       (for/vector #:length (vector-length history) ([entry (in-vector history)])
         (cons (entry->msg entry) (cdr entry))))))

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
       (:= @idx pos)]
      [(vector-empty? history)
       (:= @history (vector (new-one)))
       (:= idx 0)]))

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
      (:= @history new-entries)))
  
  (define/obs @tab "content")
  (define/obs @visible? #t)
  (define done? #f)

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
       (table '("role" "content")
              @history
              (λ (type entries idx)
                (when (and (eq? type 'select) idx)
                  (:= @idx idx)
                  (refresh-entry!)))
              #:entry->row entry->row
              #:selection @idx
              #:style '(single column-headers)
              )
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
         (tabs '("content" "reason")
               (λ (type _ sel)
                 (when (eq? type 'select)
                   (:= @tab sel)))
               (case-view
                @tab
                [("content")
                 (editor-canvas (obs-map
                                 @idx-entry
                                 (λ (e) (if e
                                            (get-text! (Msg-content (car e)) (cdr e) 'content)
                                            #f))))]
                [("reason")
                 (editor-canvas (obs-map
                                 @idx-entry
                                 (λ (e) (if e
                                            (get-text! (or (Msg-reasoning-content (car e)) "") (cdr e) 'reason)
                                            #f))))]
                [else (editor-canvas)])
               #:selection @tab)]
        [else
         (vpanel
          (text "content")
          (editor-canvas (obs-map
                          @idx-entry
                          (λ (e) (if e
                                     (get-text! (Msg-content (car e)) (cdr e) 'content)
                                     #f)))))])))
     (hpanel
      #:stretch '(#t #f) #:alignment '(center center)
      (button "Done" (λ () (set! done? #t) (:= @visible? #f)))
      (button "Cancel" (λ () (:= @visible? #f))))))
   #:wait? #t)

  (cond
    [(not done?) #f]
    [else
     (for/list ([entry (in-vector (obs-peek @history))])
       (entry->msg entry))]))

(define (input-history)
  (cond
    [(edit-history (current-history))
     => current-history]))

(module+ main
  (define t
    (list (make-user "who are you?")
          (make-assistant "I am a helpful assistant.")))
  (edit-history t))