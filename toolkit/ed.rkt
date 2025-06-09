#lang racket/base
(require racket/match racket/class racket/gui/base framework "../main.rkt")
(provide edit input redo/input)

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
  (define close-evt (make-semaphore))
  (define ok-evt (make-semaphore))
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

(define (input [init #f])
  (define content (edit init))
  (when content
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