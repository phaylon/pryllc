(declare (unit meta/number))
(declare (uses mop util))

(define <pryll:meta-number>
  (mop/init
    (mop/class name: "Number")
    (lambda (call)
      (call finalize:))))

(mop/meta-number <pryll:meta-number>)
