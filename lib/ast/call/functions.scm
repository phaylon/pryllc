(declare (unit ast/call/functions))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-function-call>
  (mop/init
    (mop/class name: "Core::AST::Call::Function")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "function-name")
            (attr/item "arguments"))
      (call add-methods:
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

