(declare (unit util))

  (import chicken scheme)
  (require-extension srfi-1 srfi-13 srfi-69 data-structures)
  (define DEBUG #f)

;  (define-record undef)
;  (define pryll:undef (make-undef))

  (define (v-true? value)
    (and value (not (void? value))))

  (define (v-false? value)
    (or (not value) (void? value)))

  (define (void? value)
    (equal? value (void)))

  (define (not-void? value)
    (not (void? value)))

  (define (phash-values hash)
    (hash-table-values hash))

  (define (phash-has hash slot)
    (hash-table-exists? hash (->string slot)))

  (define phash-slot
    (getter-with-setter
      (lambda (h key . rest)
;        (dbg "hash " h)
        (if (hash-table-exists? h key)
          (hash-table-ref h key)
          (if (null? rest)
            (void)
            ((car rest)))))
      (lambda (h key new-value)
        (dbg "hash set " h " " key " " new-value)
        (hash-table-set! h key new-value)
        new-value)))

;  (define (pryll:defined? value)
;    (not (undef? value)))

;  (define (pryll:false? value)
;    (not (pryll:true? value)))

;  (define (pryll:true? value)
;    (not (or (equal? #f value)
;             (equal? 0 value)
;             (equal? "" value)
;             (undef? value))))

  (define (dbg/pipe title value)
    (dbg title " " value)
    value)

  (define (dbg . items)
    (if DEBUG
      (begin
        (display "[debug] ")
        (map display items)
        (newline))))

  (define (list->alist ls)
;    (dbg "list->alist: " ls)
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

  (define (mkhash . alist)
;    (dbg "make hash " alist)
    (let* ((h (alist->hash-table '()
                                 test: string=?
                                 hash: string-hash
                                 size: 32)))
      (for-each (lambda (item)
                  (hash-table-set!
                    h
                    (car item)
                    (cadr item)))
                alist)
      h))

  (define (phash . ls)
    (apply mkhash (list->alist ls)))

