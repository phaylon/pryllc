(declare (unit ast/ref/methods))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-method-ref>
  (mop/init
    (mop/class name: "Core::AST::Ref::Method")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "is-maybe")
            (attr/item "invocant")
            (attr/item "method")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(ref-method
                   ,@(if (pryll:invoke self "is-maybe")
                       '(maybe:)
                       '())
                   ,(dump-slot self "invocant")
                   ,(dump-slot self "method")
                   ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-method-ref op inv met maybe args)
  (pryll:make <pryll:ast-method-ref>
              location:   (token-location op)
              is-maybe:   maybe
              invocant:   inv
              method:     met
              arguments:  args))

