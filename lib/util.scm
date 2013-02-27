
(module pryll/util
  (dbg mkhash list->alist text)
  (import chicken scheme)
  (require-extension srfi-1 srfi-13 srfi-69 data-structures)

  (define DEBUG #f)

  (define (dbg . items)
    (if DEBUG
      (begin
        (display "[debug] ")
        (map display items)
        (newline))))

  (define (list->alist ls)
    (dbg "list->alist: " ls)
    (define (tr ls done)
      (if (null? ls)
        done
        (let ((key (keyword->string (car ls)))
              (value (cadr ls))
              (rest (cddr ls)))
          (tr rest (append done (list (list key value)))))))
    (tr ls '()))

  (define (text . args)
    (string-concatenate (map ->string args)))

  (define-inline (mkhash . alist)
    (dbg "make hash " alist)
    (alist->hash-table
      alist
      test: string=?
      hash: string-hash
      size: 32))

)
