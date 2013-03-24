(declare (unit ast/documents))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-document>
  (mop/init
    (mop/class name: "Core::AST::Document")
    (lambda (call)
      (call add-attributes:
            (attr/item "statements"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile/statements
                  ctx
                  (pryll:invoke self "statements"))))
            (dump-method
              (lambda (self)
                `(doc ,(map dump
                            (pryll:invoke
                              self
                              "statements"))))))
      (call finalize:))))

(define (make-document statements)
  (pryll:make <pryll:ast-document>
              statements: statements))

