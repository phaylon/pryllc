(declare (unit meta/void))
(declare (uses mop util primitives))

(define <pryll:meta-void>
  (mop/init
    (mop/class name: "Undef")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "match"
              code: (lambda (pos nam)
                      (void? (cadr pos)))))
      (call finalize:))))

(mop/meta-void <pryll:meta-void>)
