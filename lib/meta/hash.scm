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
                      (let* ((self (v1 pos))
                             (key  (v2 pos)))
                        (hash-table-ref/default
                          self
                          key
                          (void)))))
            (mop/method
              name: "set"
              code: (lambda (pos nam)
                      (let* ((self (v1 pos))
                             (key  (v2 pos))
                             (new  (v3 pos)))
                        (hash-table-set! self key new)
                        new))))
      (call finalize:))))

(mop/meta-hash <pryll:meta-hash>)
