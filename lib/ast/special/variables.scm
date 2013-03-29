(declare (unit ast/special/variables))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-special-variable>
  (mop/init
    (mop/class name: "Core::AST::Special::Variable")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (let ((special (pryll:invoke
                                 ctx
                                 "find-special"
                                 (list
                                   (pryll:object-data self "value")))))
                  (or special
                      (pryll:err <pryll:error-syntax>
                                 location: (pryll:object-data
                                             self
                                             "location")
                                 message: (conc
                                            "Unknown special variable "
                                            "'"
                                            (pryll:object-data
                                              self
                                              "value")
                                            "'"))))))
                    
            (dump-method
              (lambda (self)
                `(special ,(string->symbol
                             (pryll:object-data self "value"))))))
      (call finalize:))))

(define (make-special-variable token)
  (pryll:make <pryll:ast-special-variable>
              location: (token-location token)
              value:    (token-value token)))
