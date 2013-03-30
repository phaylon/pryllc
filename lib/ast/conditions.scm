(declare (unit ast/conditions))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-condition>
  (mop/init
    (mop/class name: "Core::AST::Condition")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "type")
            (attr/item "condition")
            (attr/item "block")
            (attr/item "else"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                `(if (,(if (string=? (pryll:object-data self "type")
                                     "if")
                         'pryll:true?
                         'pryll:false?)
                       ,(compile ctx
                                 (pryll:object-data self "condition")))
                   ,(compile ctx
                             (pryll:object-data self "block"))
                   ,(let ((el (pryll:object-data self "else")))
                      (if (v-true? el)
                        (compile ctx el)
                        '(void))))))
            (dump-method
              (lambda (self)
                `(,(string->symbol (pryll:object-data self "type"))
                   ,(dump-slot self "condition")
                   (then ,(dump-slot self "block"))
                   ,@(let ((e (pryll:object-data self "else")))
                       (if e `((else ,(dump e))) '()))))))
      (call finalize:))))

(define (make-condition type condition block tail)
  (pryll:make <pryll:ast-condition>
              location:     (token-location type)
              type:         (token-value type)
              condition:    condition
              block:        block
              else:         tail))

