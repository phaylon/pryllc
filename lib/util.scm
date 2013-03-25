(declare (unit util))
(declare (hide DEBUG))

(import chicken scheme)
(require-extension srfi-1 srfi-13 srfi-69 data-structures)
(define DEBUG (get-environment-variable "DEBUG"))

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
 
(define (str-unknown known strings)
  (filter (lambda (str)
            (not (contains-str known str)))
          strings))

(define (contains-str ls value)
  (if (null? ls)
    #f
    (if (string=? (car ls) value)
      #t
      (contains-str (cdr ls) value))))

(define phash-slot
  (getter-with-setter
    (lambda (h key . rest)
      (if (hash-table-exists? h key)
        (hash-table-ref h key)
        (if (null? rest)
          (void)
          ((car rest)))))
    (lambda (h key new-value)
      (dbg "hash set " h " " key " " new-value)
      (hash-table-set! h key new-value)
      new-value)))

(define (dbg/pipe title value)
  (dbg title " " value)
  value)

(define (say . items)
  (display "[info] ")
  (map display items)
  (newline))

(define (dbg . items)
  (if DEBUG
    (begin
      (display "[debug] ")
      (map display items)
      (newline))))

(define (list->alist ls)
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

