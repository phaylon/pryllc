(declare (unit ast/numbers))
(declare (uses ast/util mop))

(import chicken scheme)

(define <pryll:ast-number>
  (mop/init
    (mop/class name: "Core::AST::Number")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (string->number (pryll:invoke self "value"))))
            (dump-method
              (lambda (self)
                `(num ,(pryll:invoke self "value")))))
      (call finalize:))))

(define (make-number token)
  (pryll:make <pryll:ast-number>
              location:   (token-location token)
              value:      (token-value token)))

