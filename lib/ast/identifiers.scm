(declare (unit ast/identifiers))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-identifier>
  (mop/init
    (mop/class name: "Core::AST::Identifier")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(ident ,(pryll:invoke self "value")))))
      (call finalize:))))

(define (make-identifier token)
  (pryll:make <pryll:ast-identifier>
              location:   (token-location token)
              value:      (token-value token)))

