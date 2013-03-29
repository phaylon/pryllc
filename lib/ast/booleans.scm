(declare (unit ast/booleans))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-boolean>
  (mop/init
    (mop/class name: "Core::AST::Boolean")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:     "location"
              reader:   "location"
              init-arg: "location")
            (mop/attribute
              name:     "value"
              reader:   "value"
              init-arg: "value"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (pryll:object-data self "value")))
            (dump-method
              (lambda (self)
                (pryll:object-data self "value"))))
      (call finalize:))))

(define (make-boolean token value)
  (pryll:make <pryll:ast-boolean>
              location: (token-location token)
              value:    value))
