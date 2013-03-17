(declare (unit meta/void))
(declare (uses mop util primitives))

(define <pryll:meta-void>
  (mop/init
    (mop/class name: "Void")
    (lambda (call)
      (call finalize:))))

(mop/meta-void <pryll:meta-void>)
