(declare (unit ast/documents))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-document>
  (mop/init
    (mop/class name: "Core::AST::Document")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:     "location"
              reader:   "location"
              writer:   "location")
            (attr/item "statements"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                `(pryll:stack-level
                   (pryll:stack-id
                     "top-level"
                     (void)
                     ,(car (pryll:object-data self "location")))
                   (lambda ()
                     ,(compile/statements
                        ctx
                       (pryll:invoke self "statements"))))))
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

