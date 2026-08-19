#lang racket/base
(require racket/exn racket/file racket/match "tools.rkt")
(provide (all-defined-out))

(define-tool (read_file
              [path : string #:desc "Path to the file"]
              [start_line : integer #:desc "First line to read, 1-based (default: 1)" #:def 1]
              [end_line : integer #:desc "Last line to read, 1-based inclusive (default: end of file)" #:def #f]
              [number_lines : boolean #:desc "Prefix each line with its line number and a tab character" #:def #f])
  #:desc "Read the contents of a file."
  (define s (open-output-string))
  (with-handlers ([exn:fail? (λ (e) (exn->string e))])
    (call-with-input-file path
      (λ (p)
        (for ([line (in-lines p)]
              [num (in-naturals 1)]
              #:when (>= num start_line)
              #:break (and end_line (> num end_line)))
          (when number_lines
            (fprintf s "~a\t" num))
          (fprintf s "~a\n" line))))
    (get-output-string s)))

(define-tool (write_file [path : string #:desc "Path to the file"] [content : string #:desc "Content to write"])
  #:desc "Write content to a file, creating it (including parent directories) if it does not exist."
  (with-handlers ([exn:fail? (λ (e) (exn->string e))])
    (make-parent-directory* path)
    (display-to-file content path #:exists 'truncate/replace)
    "Done"))

(define-tool (edit_file [path : string #:desc "Path to the file"]
                        [old : string #:desc "exact text to find"]
                        [new : string #:desc "text to replace old with"])
  #:desc "edit a file using exact text replacement. old must be unique in the file."
  (with-handlers ([exn:fail? (λ (e) (exn->string e))])
    (define input (file->string path))
    (define pat (regexp-quote old))
    (match (regexp-match-positions pat input)
      [(list (cons start end))
       (define new-file (string-append (substring input 0 start) new (substring input end)))
       (display-to-file new-file path #:exists 'truncate/replace)
       "Done"]
      [#f
       "old text not found"]
      [_
       "multiple occurences of old text"])))

(define (file-tools) (list read_file write_file edit_file))