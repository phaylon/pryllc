(declare (unit ast/subs))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-subroutine>
  (mop/init
    (mop/class name: "Core::AST::Subroutine")
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
                `(sub
                   ,(pryll:object-data self "name")
                   ,(let ((s (pryll:object-data self "signature")))
                      (if s (dump s) 'no-signature))
                   ,@(map dump (pryll:object-data self "traits"))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-subroutine op name signature traits block)
  (pryll:make <pryll:ast-subroutine>
              location:     (token-location op)
              name:         (pryll:invoke name "value")
              signature:    signature
              traits:       traits
              block:        block))

