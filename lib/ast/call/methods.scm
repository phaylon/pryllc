(declare (unit ast/call/methods))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-method-call>
  (mop/init
    (mop/class name: "Core::AST::Call::Method")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "is-maybe")
            (attr/item "is-chained")
            (attr/item "invocant")
            (attr/item "method")
            (attr/item "arguments"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (let ((var-inv (compile/genvar 'invocant))
                      (var-met (compile/genvar 'method))
                      (inv     (pryll:object-data self "invocant"))
                      (met     (pryll:object-data self "method"))
                      (chain   (pryll:object-data self "is-chained"))
                      (maybe   (pryll:object-data self "is-maybe"))
                      (args    (pryll:object-data self "arguments")))
                  `(let ((,var-inv ,(compile ctx inv))
                         (,var-met ,(compile ctx met)))
                     (,(if maybe
                         'pryll:invoke/maybe
                         'pryll:invoke)
                       ,var-inv
                       ,var-met
                       ,(pryll:invoke
                          args
                          "compile-positional"
                          (vector ctx))
                       ,(pryll:invoke
                          args
                          "compile-named"
                          (vector ctx))
                       (list ,@(pryll:object-data self "location")))
                     ,@(if chain
                         (list var-inv)
                         '())))))
            (dump-method
              (lambda (self)
                `(call-method
                   ,@(if (pryll:invoke self "is-maybe")
                       '(maybe)
                       '())
                   ,@(if (pryll:invoke self "is-chained")
                       '(chained)
                       '())
                   ,(dump-slot self "invocant")
                   ,(dump-slot self "method")
                   ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-method-call op inv met maybe chained args)
  (pryll:make <pryll:ast-method-call>
              location:       (token-location op)
              is-maybe:       maybe
              is-chained:     chained
              invocant:       inv
              method:         met
              arguments:      args))

