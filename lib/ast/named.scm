(declare (unit ast/named))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-named-value>
  (mop/init
    (mop/class name: "Core::AST::Named")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(named
                   ,(dump-slot self "name")
                   ,(dump-slot self "value")))))
      (call finalize:))))

(define (make-named-value op name value)
  (pryll:make <pryll:ast-named-value>
              location:   (token-location op)
              name:       name
              value:      value))

(define (named-value? item)
  (pryll:isa? item <pryll:ast-named-value>))

