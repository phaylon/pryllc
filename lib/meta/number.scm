(declare (unit meta/number))
(declare (uses mop util))

(define <pryll:meta-number>
  (mop/init
    (mop/class name: "Number")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "match"
              code: (lambda (pos nam)
                      (let* ((self (car pos))
                             (value (cadr pos)))
                        (and (number? value)
                             (= self value)))))
            (mop/method
              name: "str"
              code: (lambda (pos nam)
                      (number->string (car pos))))
            (mop/method
              name: "num"
              code: (lambda (pos nam) (car pos))))
      (call finalize:))))

(define <pryll:meta-integer>
  (mop/init
    (mop/class name: "Integer" extends: <pryll:meta-number>)
    (lambda (call)
      (call finalize:))))

(mop/meta-number <pryll:meta-number>)
(mop/meta-integer <pryll:meta-integer>)
