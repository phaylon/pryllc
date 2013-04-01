(declare (unit ast/lambdas))
(declare (uses ast/util mop compiler))

(define-inline (compile-lambda ctx type descr location signature block)
  (let ((var-pos (compile/genvar 'pos))
        (var-nam (compile/genvar 'nam))
        (var-none (compile/genvar 'none)))
    (if signature
      `(lambda (,var-pos ,var-nam)
         ,(compile-with-return
            ctx
            (lambda (rctx)
              `(pryll:stack-level
                 (pryll:stack-id
                   ,(if (string? type) type "lambda")
                   (pryll:caller-location)
                   ,(if (string? descr) descr ""))
                 (lambda ()
                   ,(pryll:invoke
                      signature
                      "compile-scope"
                      (vector rctx var-pos var-nam block))
                   (void))))))
      `(lambda ,var-none
         ,(compile-with-return
            ctx
            (lambda (rctx)
              `(pryll:stack-level
                 (pryll:stack-id
                   ,(if (string? type) type "lambda")
                   (pryll:caller-location)
                   ,(if (string? descr) descr ""))
                 (lambda ()
                   ,(compile rctx block)
                   (void)))))))))

(define <pryll:ast-lambda>
  (mop/init
    (mop/class name: "Core::AST::Lambda")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "signature")
            (attr/item "stack-type")
            (attr/item "stack-description")
            (attr/item "block"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile-lambda
                  ctx
                  (pryll:object-data self "stack-type")
                  (pryll:object-data self "stack-description")
                  (pryll:object-data self "location")
                  (pryll:object-data self "signature")
                  (pryll:object-data self "block"))))
            (dump-method
              (lambda (self)
                `(lambda
                   ,(let ((signature
                            (pryll:object-data self "signature")))
                      (if signature
                        (dump signature)
                        'no-signature))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-lambda token signature block)
  (pryll:make <pryll:ast-lambda>
              location:  (token-location token)
              signature: signature
              block:     block))
              
(define <pryll:ast-return>
  (mop/init
    (mop/class name: "Core::AST::Return")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "expression"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (let ((var-ret (pryll:invoke ctx "find-return")))
                  (if var-ret
                    `(,var-ret
                       ,(compile
                          ctx
                          (pryll:object-data self "expression")))
                    (pryll:err <pryll:error-syntax>
                               location: (pryll:object-data
                                           self
                                           "location")
                               message: (conc
                                          "Invalid return outside of "
                                          "lambda scope"))))))
            (dump-method
              (lambda (self)
                `(return ,(dump-slot self "expression")))))
      (call finalize:))))

(define (make-return op expr)
  (pryll:make <pryll:ast-return>
              location:     (token-location op)
              expression:   expr))

