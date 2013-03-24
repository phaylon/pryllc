(declare (unit ast/functions))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-function>
  (mop/init
    (mop/class name: "Core::AST::Function")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "signature")
            (attr/item "traits")
            (attr/item "block"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(function
                   ,(pryll:object-data self "name")
                   ,(let ((s (pryll:object-data self "signature")))
                      (if s (dump s) 'no-signature))
                   ,@(map dump (pryll:object-data self "traits"))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-function op name signature traits block)
  (pryll:make <pryll:ast-function>
              location:     (token-location op)
              name:         (pryll:invoke name "value")
              signature:    signature
              traits:       traits
              block:        block))

