(declare (unit primitives))
(declare (uses mop util errors))

(import chicken scheme)
(require-extension extras srfi-1 srfi-13 srfi-69 data-structures)

(define-inline (assert-type pred meta value)
  (if (pred value)
    value
    (pryll:err <pryll:error-type>
               expected: meta
               received: (pryll:meta-for value))))

(define (assert/hash value)
  (assert-type hash-table? <pryll:meta-hash> value))

(define (assert/string value)
  (assert-type string? <pryll:meta-string> value))

(define (assert/number value)
  (assert-type number? <pryll:meta-number> value))

(define (assert/integer value)
  (assert-type integer? <pryll:meta-integer> value))


;;
;; hashes
;;

(define p/hash-ref
  (getter-with-setter
    (lambda (ht key)
      (assert/hash ht)
      (assert/string key)
      (hash-table-ref/default ht key (void)))
    (lambda (ht key new-value)
      (assert/hash ht)
      (assert/string key)
      (hash-table-set! ht key new-value)
      new-value)))

;;
;; arrays
;;

(define-inline (list-last ls)
  (- (length ls) 1))

(define-inline (list-void slot-num)
  (define (rep num built)
    (if (= num 0)
      built
      (rep (- num 1) (cons (void) built))))
  (rep slot-num '()))

(define-inline (negative-index array index)
  (let ((neg-index (abs index)))
    (if (<= neg-index (length array))
      (- (length array) neg-index)
      (pryll:err <pryll:error-generic>
                 message:
                 (sprintf
                   (conc "Cannot access array via negative slot "
                         "index ~a, the array has only ~a elements")
                   index
                   (length array))))))

;(define p/array-ref
;  (getter-with-setter
;    (lambda (array index)
;      (pryll:array/assert array)
;      (assert/integer index)
;      (if (>= index 0)
;        (if (> index (list-last array))
;          (void)
;          (list-ref array index))
;        (let ((neg-index (negative-index array index)))
;          (list-ref array neg-index))))
;    (lambda (array index new-value)
;      (pryll:array/assert array)
;      (assert/number index)
;      (if (>= index 0)
;        (let ((diff (- (list-last array) index)))
;          (if (< diff 0)
;            (append! array (list-void (abs diff))))
;          (set! (list-ref array index) new-value))
;        (let ((neg-index (negative-index array index)))
;          (set! (list-ref array neg-index) new-value)))
;      new-value)))
