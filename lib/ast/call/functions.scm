(declare (unit ast/call/functions))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define-inline (compile-func-call self ctx)
  (let* ((name (pryll:object-data self "function-name"))
         (loc  (pryll:object-data self "location"))
         (args (pryll:object-data self "arguments"))
         (var  (pryll:invoke ctx "find-callable" (list name))))
    (if var
      `(pryll:call
         ,var
         ,(pryll:invoke args "compile-positional" (list ctx))
         ,(pryll:invoke args "compile-named" (list ctx))
         (list ,@loc))
      (pryll:err <pryll:error-syntax>
                 location: loc
                 message:  (conc "Unknown callable "
                                 "'"
                                 name
                                 "'")))))

(define <pryll:ast-function-call>
  (mop/init
    (mop/class name: "Core::AST::Call::Function")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "function-name")
            (attr/item "arguments"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile-func-call self ctx)))
            (dump-method
              (lambda (self)
                `(func-call ,(pryll:invoke self "function-name")
                            ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-function-call name args)
  (pryll:make <pryll:ast-function-call>
              location:         (token-location name)
              function-name:    (token-value name)
              arguments:        args))

