(declare (unit meta/string))
(declare (uses mop util))

(define <pryll:meta-string>
  (mop/init
    (mop/class name: "String")
    (lambda (call)
      (call finalize:))))

(mop/meta-string <pryll:meta-string>)
