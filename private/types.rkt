#lang typed/racket/base/shallow
(provide (all-defined-out))

(define-type JSExpr
  (U 'null Boolean String Integer Inexact-Real (Listof JSExpr) (HashTable Symbol JSExpr)))

(require/typed/provide
 json
 [jsexpr->string (-> JSExpr String)]
 [jsexpr->bytes (-> JSExpr Bytes)]
 [string->jsexpr (-> String JSExpr)]
 [bytes->jsexpr (-> Bytes JSExpr)])

(require/typed/provide
 net/base64
 [base64-encode (-> Bytes Bytes)])

(require/typed/provide
 net/url
 [#:opaque URL url?]
 [string->url (-> String URL)]
 [http-sendrecv/url
  (URL
   [#:method (U Bytes String Symbol)]
   [#:headers (Listof (U Bytes String))]
   [#:data (Option (U Bytes String))]
   [#:content-decode (Listof Symbol)]
   -> (Values Bytes (Listof Bytes) Input-Port))])
