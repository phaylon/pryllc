(declare (unit util))
(declare (hide DEBUG))

(import chicken scheme)
(require-extension srfi-1 srfi-13 srfi-69 data-structures vector-lib)
(define DEBUG (get-environment-variable "DEBUG"))

(define (v1 v) (vector-ref v 0))
(define (v2 v) (vector-ref v 1))
(define (v3 v) (vector-ref v 2))
(define (v4 v) (vector-ref v 3))

(define (group proc ls wtrue wfalse)
  (let* ((prev (void))
         (gtrue (list))
         (gfalse (list))
         (done (list))
         (buf (list)))
    (for-each (lambda (item)
                (let* ((res (proc item)))
                  (if (void? prev)
                    (begin
                      (set! prev res)
                      (set! buf (cons item buf)))
                    (begin
                      (if (equal? prev res)
                        (set! buf (cons item buf))
                        (begin
                          (set! done
                            (cons
                              (apply
                                (if prev wtrue wfalse)
                                (list (reverse buf)))
                              done))
                          (set! buf (list item))
                          (set! prev res)))))))
              ls)
    (if (not (null? buf))
      (set! done
        (cons
          (apply
            (if prev wtrue wfalse)
            (list (reverse buf)))
          done)))
    (reverse done)))

(define (count proc ls)
  (length (filter proc ls)))

(define (in-list proc ls)
  (< 0 (count proc ls)))

(define (vtail v #!optional index)
  (let ((i (or index 1)))
    (if (> (vector-length v) i)
      (subvector v i)
      (vector))))

(define (vnull? v)
  (= 0 (vector-length v)))

(define (vcons a b)
  (vector-append (vector a) b))

(define (pryll:false? value)
  (not (pryll:true? value)))

(define (pryll:true? value)
  (not (or (and (boolean? value)
                (not value))
           (void? value)
           (and (number? value)
                (= 0 value))
           (and (string? value)
                (string-null? value)))))

(define (v-true? value)
  (and value (not (void? value))))

(define (v-false? value)
  (or (not value) (void? value)))

(define (void? value)
  (equal? value (void)))

(define (not-void? value)
  (not (void? value)))

(define (phash-values hash)
  (list->vector (hash-table-values hash)))

(define (phash-has hash slot)
  (hash-table-exists? hash (->string slot)))
 
(define (str-unknown known strings)
  (filter (lambda (str)
            (not (contains-str known str)))
          strings))

(define (contains-str src value)
  (let ((ls (if (vector? src) (vector->list src) src)))
    (if (null? ls)
      #f
      (if (string=? (car ls) value)
        #t
        (contains-str (cdr ls) value)))))

(define phash-slot
  (getter-with-setter
    (lambda (h key . rest)
      (if (hash-table-exists? h key)
        (hash-table-ref h key)
        (if (null? rest)
          (void)
          ((car rest)))))
    (lambda (h key new-value)
;      (dbg "hash set " h " " key " " new-value)
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

(define (phash . src)
  (let ((ls (if (vector? src) (vector->list src) src)))
    (apply mkhash (list->alist ls))))

