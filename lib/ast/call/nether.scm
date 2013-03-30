(declare (unit ast/call/nether))
(declare (uses ast/util mop compiler errors nether))

(import chicken scheme)

(define <pryll:ast-nether-call>
  (mop/init
    (mop/class name: "Core::AST::Call::Nether")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "is-chained")
            (attr/item "invocant")
            (attr/item "method")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(nether-call
                   ,@(if (pryll:invoke self "is-chained")
                       '(chained)
                       '())
                   ,(dump-slot self "invocant")
                   ,(dump-slot self "method")
                   ,(dump-slot self "arguments"))))
            (compile-method
              (lambda (self ctx)
                (let* ((method (pryll:object-data
                                 (pryll:object-data self "method")
                                 "value"))
                       (nether (nether-method method))
                       (var-inv (compile/genvar 'invocant))
                       (inv (pryll:object-data self "invocant"))
                       (args (pryll:object-data self "arguments"))
                       (chain (pryll:object-data self "is-chained"))
                       (loc (pryll:object-data self "location")))
                  (if nether
                    `(let ((,var-inv ,(compile ctx inv)))
                       (,nether
                         (append
                           (list ,var-inv)
                           ,(pryll:invoke
                              args
                              "compile-positional"
                              (list ctx)))
                         ,(pryll:invoke
                            args
                            "compile-named"
                            (list ctx)))
                       ,@(if chain
                           (list var-inv)
                           '()))
                    (pryll:err
                      <pryll:error-syntax>
                      location: loc
                      message: (sprintf "Invalid nether method ~s"
                                        method)))))))
      (call finalize:))))

(define (make-nether-call op inv met chained args)
  (pryll:make <pryll:ast-nether-call>
              location:       (token-location op)
              is-chained:     chained
              invocant:       inv
              method:         met
              arguments:      args))

