(declare (unit ast/call))
(declare (uses ast/util mop))

(import chicken scheme)

(define <pryll:ast-call>
  (mop/init
    (mop/class name: "Core::AST::Call")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "function")
            (attr/item "arguments"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (let ((args (pryll:object-data self "arguments")))
                `(pryll:call
                   ,(compile ctx (pryll:object-data self "function"))
                   ,(pryll:invoke args "compile-positional" (list ctx))
                   ,(pryll:invoke args "compile-named" (list ctx))
                   (list ,@(pryll:object-data self "location"))))))
            (dump-method
              (lambda (self)
                `(call ,(dump-slot self "function")
                       ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-call op func args)
  (pryll:make <pryll:ast-call>
              location:   (token-location op)
              function:   func
              arguments:  args))

