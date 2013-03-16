(declare (unit meta/array))
(declare (uses mop util))

(define <pryll:meta-array>
  (mop/init
    (mop/class name: "Array")
    (lambda (call)
      (call finalize:))))
