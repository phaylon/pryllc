(declare (unit ast/slot))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define-inline (compile-slot-access self ctx method extra)
  (let ((var-cont (compile/genvar 'container))
        (var-slot (compile/genvar 'slot))
        (loc (pryll:object-data self "location"))
        (cont (pryll:object-data self "container"))
        (slot (pryll:object-data self "slot")))
    `(let ((,var-cont ,(compile ctx cont))
           (,var-slot ,(compile ctx slot)))
       (pryll:invoke
         ,var-cont
         ,method
         (list ,var-slot ,@extra)))))
  
(define <pryll:ast-slot-ref>
  (mop/init
    (mop/class name: "Core::AST::SlotRef")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "container")
            (attr/item "slot"))
      (call add-methods:
            (mop/method
              name: "compile-set"
              code: (unwrap-pos-args
                      (lambda (self ctx new-value)
                        (compile-slot-access
                          self
                          ctx
                          "set"
                          (list (compile ctx new-value))))))
            (compile-method
              (lambda (self ctx)
                (compile-slot-access self ctx "get" '())))
            (dump-method
              (lambda (self)
                `(slot
                   ,(dump-slot self "container")
                   ,(dump-slot self "slot")))))
      (call finalize:))))

(define (make-slot-ref op container slot)
  (pryll:make <pryll:ast-slot-ref>
              location:   (token-location op)
              container:  container
              slot:       slot))

