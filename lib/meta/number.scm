(declare (unit meta/number))
(declare (uses mop util))

(define <pryll:meta-number>
  (mop/init
    (mop/class name: "Number")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "num"
              code: (lambda (pos nam) (car pos))))
      (call finalize:))))

(mop/meta-number <pryll:meta-number>)
