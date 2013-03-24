(declare (unit ast/barewords))
(declare (uses ast/util mop))

(import chicken scheme)

(define <pryll:ast-bareword>
  (mop/init
    (mop/class name: "Core::AST::Bareword")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                (string->symbol (pryll:invoke self "value")))))
      (call finalize:))))

(define (make-bareword token)
  (pryll:make <pryll:ast-bareword>
              location:   (token-location token)
              value:      (token-value token)))

