(declare (unit meta/array))
(declare (uses mop util primitives))

(define <pryll:meta-array>
  (mop/init
    (mop/class name: "Array")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "map"
              code: (lambda (pos nam)
                      (let ((self (car pos))
                            (proc (cadr pos)))
                        (map (lambda (item)
                               (pryll:call proc (list item)))
                             self))))
            (mop/method
              name: "set"
              code: (lambda (pos nam)
                      (set! (p/array-ref (car pos) (cadr pos))
                        (caddr pos))
                      (caddr pos)))
            (mop/method
              name: "get"
              code: (lambda (pos nam)
                      (p/array-ref (car pos) (cadr pos)))))
      (call finalize:))))

(mop/meta-array <pryll:meta-array>)
