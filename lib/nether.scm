(declare (unit nether))
(declare (uses mop util errors))

(declare (hide nether-methods))
(define nether-methods
  `(("meta"     . nether/meta)
    ("array"    . nether/array)
    ))

(define (nether-method name)
  (let ((found (assoc name nether-methods)))
    (if found
      (cdr found)
      #f)))

(define (nether/array pos nam)
  (let ((value (car pos)))
    (if (list? value)
      value
      (list value))))

(define (nether/meta pos nam)
  (pryll:meta-for (car pos)))
