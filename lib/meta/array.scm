(declare (unit meta/array))
(declare (uses mop util primitives))

(define <pryll:meta-array>
  (mop/init
    (mop/class name: "Array")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "set"
              code: (lambda (pos nam)
                      (let* ((array (assert/array (car pos)))
                             (index (assert/number (cadr pos))))
                        (set! (p/array-ref array index) (caddr pos)))))
            (mop/method
              name: "get"
              code: (lambda (pos nam)
                      (let* ((array (assert/array (car pos)))
                             (index (assert/number (cadr pos))))
                        (p/array-ref array index)))))
      (call finalize:))))

(mop/meta-array <pryll:meta-array>)
