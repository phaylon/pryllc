(declare (unit meta/array))
(declare (uses mop util primitives))

(require-extension vector-lib utils lolevel)

(define (pryll:array . items)
  (pryll:array-make items))

(define (pryll:array-make items)
  (apply vector items))

(define (pryll:array/assert value)
  (assert-type vector? <pryll:meta-array> value))

(define (pryll:array-append . args)
  (apply vector-append args))

(define (pryll:array-map proc vec)
  (vector-map proc vec))

(define-inline (calc-index vec idx)
  (if (>= idx 0)
    idx
    (let ((size (vector-length vec))
          (real (- size (abs idx))))
      (if (> (+ real 1) size)
        (pryll:err <pryll:error-generic>
                   message: (conc "Not enough elements in Array to "
                                  "access index "
                                  idx))
        real))))

(define <pryll:meta-array>
  (mop/init
    (mop/class name: "Array")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "push"
              code: (lambda (pos nam)
                      (let ((self (v1 pos))
                            (items (vtail pos)))
                        (object-become!
                          (list
                            (cons self
                                  (vector-append self items)))))))
            (mop/method
              name: "map"
              code: (lambda (pos nam)
                      (let ((self (v1 pos))
                            (proc (v2 pos)))
                        (vector-map
                          (lambda (i item)
                            (pryll:call proc (vector item)))
                          self))))
            (mop/method
              name: "set"
              code: (lambda (pos nam)
                      (let* ((self (v1 pos))
                             (idx  (v2 pos))
                             (real (calc-index self idx))
                             (size (vector-length self))
                             (new  (v3 pos)))
                        (if (> (+ real 1) size)
                          (object-become!
                            (list
                              (cons self
                                    (vector-resize self (+ real 1))))))
                        (vector-set! self real new)
                        new)))
            (mop/method
              name: "get"
              code: (lambda (pos nam)
                      (let* ((self (v1 pos))
                             (idx  (v2 pos))
                             (real (calc-index self idx))
                             (size (vector-length self)))
                        (if (> (+ real 1) size)
                          (void)
                          (vector-ref self real))))))
      (call finalize:))))

(mop/meta-array <pryll:meta-array>)
