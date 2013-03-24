(declare (unit ast/blocks))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-block>
  (mop/init
    (mop/class name: "Core::AST::Block")
    (lambda (call)
      (call add-attributes:
            (attr/item "statements"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile/statements
                  ctx
                  (pryll:object-data self "statements"))))
            (dump-method
              (lambda (self)
                `(block
                   ,@(map dump (pryll:object-data self "statements"))))))
      (call finalize:))))

(define (make-block statements)
  (pryll:make <pryll:ast-block>
              statements: statements))

