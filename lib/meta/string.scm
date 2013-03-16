(declare (unit meta/string))
(declare (uses mop util))

(define <pryll:meta-string>
  (mop/init
    (mop/class name: "String")
    (lambda (call)
      (call add-methods:
            (mop/method
              name: "str"
              code: (lambda (pos nam)
                      (car pos)))
            (mop/method
              name: "num"
              code: (lambda (pos nam)
                      (let* ((self (car pos))
                             (str  self)
                             (num  (string->number str)))
                        (or num
                            (pryll:err
                              <pryll:error-coercion>
                              message:   (conc
                                           "The string "
                                           "'"
                                           str
                                           "' does not represent a "
                                           "valid numeric sequence")
                              from-type: <pryll:meta-string>
                              to-type:   <pryll:meta-number>))))))
      (call finalize:))))

(mop/meta-string <pryll:meta-string>)
