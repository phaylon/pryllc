(declare (unit ast/operators/unary))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-unary-operator>
  (let ((unops
        `(("-" ,(lambda (self ctx operand)
                  (let ((c-operand (pryll:invoke
                                     operand
                                     "compile"
                                     (list ctx))))
                    (if (number? c-operand)
                      (* c-operand -1)
                      (pryll:err
                        <pryll:error-syntax>
                        message: (conc "Unary negation operator '-' "
                                       "(minus) can only be used on "
                                       "numbers")))))))))
    (mop/init
      (mop/class name: "Core::AST::Operator::Binary")
      (lambda (call)
        (call add-attributes:
              (attr/item "location")
              (attr/item "operator")
              (attr/item "position")
              (attr/item "operand"))
        (call add-methods:
              (compile-method
                (lambda (self ctx)
                  (let ((op-comp (cadr (assoc
                                         (pryll:invoke
                                           self
                                           "operator")
                                         unops))))
                    (op-comp
                      self
                      ctx
                      (pryll:invoke self "operand")))))
              (dump-method
                (lambda (self)
                  `(unop
                     ,(pryll:invoke self "operator")
                     ,(dump (pryll:invoke self "operand"))))))
        (call finalize:)))))

(define (make-unary-operator op operand pos)
  (pryll:make <pryll:ast-unary-operator>
              location:   (token-location op)
              operator:   (token-value op)
              operand:    operand
              position:   pos))

