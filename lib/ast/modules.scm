(declare (unit ast/modules))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-module>
  (mop/init
    (mop/class name: "Core::AST::Module")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "traits")
            (attr/item "block"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(module
                   ,(dump-slot self "name")
                   ,@(map dump (pryll:object-data self "traits"))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-module op name traits block)
  (pryll:make <pryll:ast-module>
              location: (token-location op)
              traits:   traits
              name:     name
              block:    block))

