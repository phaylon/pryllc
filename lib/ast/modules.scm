(declare (unit ast/modules))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define-inline (compile-module self ctx)
  (let* ((name (pryll:invoke (pryll:object-data self "name") "name")))
    (compile/with-special-var
      ctx
      "$*MODULE"
      `(mop/module name: ,name)
      (lambda (mctx var)
        (compile/with-namespace
          mctx
          name
          (lambda (nsctx)
            `(begin
               ,(compile/declaration nsctx self var)
               ,(compile
                  nsctx
                  (pryll:object-data self "block"))
               (pryll:invoke ,var "finalize")
               (pryll:register ,var))))))))

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
            (compile-method
              (lambda (self ctx)
                (compile-module self ctx)))
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

