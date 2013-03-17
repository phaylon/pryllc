(declare (unit meta/hash))
(declare (uses mop util primitives))

(define <pryll:meta-hash>
  (mop/init
    (mop/class name: "Hash")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "get"
              code: (lambda (pos nam)
                      (p/hash-ref (car pos) (cadr pos))))
            (mop/method
              name: "set"
              code: (lambda (pos nam)
                      (set! (p/hash-ref (car pos) (cadr pos))
                        (caddr pos))
                      (caddr pos))))
      (call finalize:))))

(mop/meta-hash <pryll:meta-hash>)
