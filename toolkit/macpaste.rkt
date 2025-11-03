#lang racket/base
(module clipboard racket/base
  (require (for-syntax racket/base syntax/parse) racket/list)
  #;(require racket/gui/base)
  (require ffi/unsafe/objc ffi/unsafe/nsalloc ffi/unsafe/atomic
           ffi/unsafe ffi/unsafe/nsstring)
  (provide do-paste PNG JPEG WEBP)

  (import-class NSPasteboard NSAttributedString)
  (define _NSUInteger _ulong)
  (define-cstruct _NSRange ([location _NSUInteger]
                            [length _NSUInteger]))

  (define-syntax-rule (atomically expr ...)
    (begin
      (start-atomic) 
      (begin0 (let () expr ...)
              (end-atomic))))

  (define-sequence-syntax in-nsarray
    (λ () (raise-syntax-error 'in-nsarray "todo"))
    (λ (stx)
      (syntax-parse stx
        [[(d) (_ (~optional (~seq #:type T)) obj)]
         #'[(d)
            (:do-in
             ([(arr count) (let ([arr obj])
                             (values arr (tell #:type _NSUInteger arr count)))])
             (void)
             ([i 0])
             (< i count)
             ([(d) (tell (~? (~@ #:type T)) arr objectAtIndex: #:type _NSUInteger i)])
             #t
             #t
             [(+ i 1)])]]
        [_ #f])))

  (define-sequence-syntax in-attr-str
    (λ () (raise-syntax-error 'in-attr-str "todo"))
    (λ (stx)
      (syntax-parse stx
        [[(attrs rng) (_ obj)]
         #'[(attrs rng)
            (:do-in
             ([(attr-str len) (let ([attr-str obj])
                                (values attr-str (tell #:type _NSUInteger attr-str length)))])
             (void)
             ([i 0])
             (< i len)
             ([(attrs rng) (let ([rng (make-NSRange 0 0)])
                             (values (tell attr-str attributesAtIndex: #:type _NSUInteger i effectiveRange: #:type _NSRange-pointer rng)
                                     rng))])
             #t
             #t
             [(+ (NSRange-location rng) (NSRange-length rng))])]]
        [_ #f])))

  (define PNG "public.png")
  (define JPEG "public.jpeg")
  (define WEBP "org.webmproject.webp")
  (define TEXT "public.utf8-plain-text")
  (define FLAT-RTFD "com.apple.flat-rtfd")

  (define (get-item-types item)
    (for/list ([type (in-nsarray #:type _NSString (tell item types))])
      type))

  (define (get-data-bytes data)
    (let ([len (tell #:type _NSUInteger data length)]
          [bstr (tell #:type _pointer data bytes)])
      (define result (make-bytes len))
      (memcpy result bstr len)
      result))

  (define (handle-rtfd data)
    (define attr-str (tell (tell NSAttributedString alloc) initWithRTFD: data documentAttributes: #f))
    (cond
      [(not attr-str) '()]
      [else
       (for/list ([(attrs rng) (in-attr-str attr-str)])
         (define sub-attr-str (tell attr-str attributedSubstringFromRange: #:type _NSRange rng))
         (define str (tell #:type _NSString sub-attr-str string))
         (define attachment (tell attrs valueForKey: #:type _NSString "NSAttachment"))
         (cond
           [attachment
            (define file-type (tell #:type _NSString attachment fileType))
            (define content (tell attachment contents))
            (define file-wrapper (tell attachment fileWrapper))
            (cons
             str
             (cond
               [content (vector file-type (get-data-bytes content))]
               [file-wrapper (vector file-type (get-data-bytes (tell file-wrapper regularFileContents)))]
               [else '()]))]
           [else
            str])
         #;
         (for ([str (in-nsarray #:type _NSString (tell attrs allKeys))])
           (displayln str))
         )]))

  (define (do-paste)
    (flatten
     (atomically
      (with-autorelease
          (define pb (tell NSPasteboard generalPasteboard))
        (define items (tell pb pasteboardItems))
        (cond
          [(not items) '()]
          [else
           (for/list ([item (in-nsarray items)]
                      #:when item)
             (define types (get-item-types item))
             (cond
               [(member PNG types)
                (define data (tell item dataForType: #:type _NSString PNG))
                (if (not data) '() (vector PNG (get-data-bytes data)))]
               [(member FLAT-RTFD types)
                (define data (tell item dataForType: #:type _NSString FLAT-RTFD))
                (handle-rtfd data)
                #;(if (not data) '() (vector FLAT-RTFD (get-data-bytes data)))]
               [else
                (tell #:type _NSString item stringForType: #:type _NSString TEXT)]))]))))))

(require "../private/main.rkt" 'clipboard)
(provide paste)

(define (paste)
  (define contents (do-paste))
  (for/list ([item contents])
    (cond
      [(string? item) item]
      [(member (vector-ref item 0) (list PNG JPEG WEBP))
       (Image (vector-ref item 1))]
      [else ""])))