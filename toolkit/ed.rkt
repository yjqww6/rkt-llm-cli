#lang racket/base
(require racket/match racket/list racket/class racket/gui/base framework "../main.rkt")
(provide edit input redo/input edit-history input-history prev-input)

(define (get-content text)
  (let loop ([s (send text find-first-snip)]
             [out (open-output-string)])
    (define (flush c)
      (define str (get-output-string out))
      (if (= 0 (string-length str))
          c
          (cons str c)))
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

(define (edit [init #f])
  (define f (new dialog%
                 [label "Editor"]
                 [width 480]
                 [height 640]
                 [style '(resize-border close-button)]))
  (define c (new editor-canvas% [parent f]))
  (define t (new text%))
  (when init
    (load-content t init))
  (define k (new keymap%))
  (keymap:setup-global k)
  (add-editor-keymap-functions k)
  (add-text-keymap-functions k)
  ((current-text-keymap-initializer) k)
  (send t set-keymap k)
  (send c set-editor t)
  (send t set-max-undo-history 100)
  (define submitted? #f)

  (gui-utils:ok/cancel-buttons
   f
   (λ (b e) (set! submitted? #t) (send f show #f))
   (λ (b e) (send f show #f)))
  (send f show #t)
  (and submitted?
       (get-content t)))

(define prev-input (make-parameter #f))

(define (input [init #f])
  (define content (edit init))
  (when content
    (prev-input content)
    (repl-chat content)))

(define (redo/input)
  (match (current-history)
    [(list old ... (and u (struct* Msg ([role "user"] [content c]))) _)
     (define new-content (edit c))
     (when new-content
       (define new-history
         (parameterize ([current-history old])
           (repl-chat new-content)
           (current-history)))
       (unless (eq? new-history old)
         (current-history new-history)))]))

(define (edit-history history)
  (define f (new dialog%
                 [label "Editor"]
                 [width 480]
                 [height 640]
                 [style '(resize-border close-button)]))
  (define h (new horizontal-pane% [parent f]))
  (define on-select
    (λ (l e)
      (cond
        [(send l get-selection)
         =>
         (λ (s)
           (send c set-editor (hash-ref texts (send l get-data s))))])))
  (define v (new vertical-pane% [parent h] [stretchable-width #f] [min-width 100]))
  (define l (new list-box% [label ""] [choices '()] [parent v] [callback on-select]))
  (define c (new editor-canvas% [parent h]))
  (define r (new choice% [label "Role"] [choices '("user" "assistant")] [parent v]))
  (define texts (make-weak-hasheq))
  
  (define k (new keymap%))
  (keymap:setup-global k)
  (add-editor-keymap-functions k)
  (add-text-keymap-functions k)
  ((current-text-keymap-initializer) k)
  
  (define (new-text! msg)
    (define t (new text%))
    (send t set-keymap k)
    (send t set-max-undo-history 100)
    (load-content t (Msg-content msg))
    (hash-set! texts msg t))
  
  (define (adjust f)
    (define n (send l get-number))
    (define history
      (for/list ([i (in-range (send l get-number))])
        (send l get-data i)))
    (define-values (new-history new-s) (f history (send l get-selection)))
    (send l clear)
    (for ([msg (in-list new-history)])
      (send l append (Msg-role msg) msg))
    (when new-s
      (send l select new-s)
      (on-select l #f)))
  
  (define (make-msg)
    (define msg
      (match (send r get-string-selection)
        ["user" (make-user "")]
        ["assistant" (make-assistant "")]))
    (new-text! msg)
    msg)
  
  (new button% [label "Insert Before"] [parent v]
       [callback (λ (b e)
                   (adjust (λ (h s)
                             (cond
                               [(not s) (values (cons (make-msg) h) 0)]
                               [else
                                (define-values (a b) (split-at h s))
                                (values (append a (list (make-msg)) b) s)]))))])
  
  (new button% [label "Insert After"] [parent v]
       [callback (λ (b e)
                   (adjust (λ (h s)
                             (cond
                               [(not s) (values (append h (list (make-msg))) (length h))]
                               [else
                                (define-values (a b) (split-at h (+ s 1)))
                                (values (append a (list (make-msg)) b) (+ s 1))]))))])
  (new button% [label "Delete"] [parent v]
       [callback (λ (b e)
                   (define s (send l get-selection))
                   (when s (send l delete s)))])

  (define ok? #f)
  (define-values (ok-button cancel-button)
    (gui-utils:ok/cancel-buttons
     f
     (λ (b e) (set! ok? #t) (send f show #f))
     (λ (b e) (send f show #f))
     "OK"
     "Cancel"))
  
  (for ([msg (in-list history)])
    (define new-msg (struct-copy Msg msg))
    (new-text! new-msg)
    (send l append (Msg-role msg) new-msg))
  (when (> (length history) 0)
    (send l select (- (length history) 1))
    (on-select l #f))
  (send f show #t)
  (and ok?
       (for/list ([i (in-range (send l get-number))])
         (define msg (send l get-data i))
         (struct-copy Msg msg [content (get-content (hash-ref texts msg))]))))

(define (input-history)
  (cond
    [(edit-history (current-history))
     => current-history]))