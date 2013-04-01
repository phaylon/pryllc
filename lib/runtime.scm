(declare (unit runtime))

(define (func/say pos nam)
  (vector-map (lambda (i item) (display item)) pos)
  (newline)
  (void))

(define (func/print pos nam)
  (vector-map (lambda (i item) (display item)) pos)
  (void))
