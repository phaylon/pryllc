(declare (unit ast/traits))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-trait>
  (mop/init
    (mop/class name: "Core::AST::Trait")
    (lambda (call)
      (call add-attributes:
            (attr/item "name")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(trait
                   ,(dump-slot self "name")
                   ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-trait name arguments)
  (pryll:make <pryll:ast-trait>
              name:         name
              arguments:    arguments))

