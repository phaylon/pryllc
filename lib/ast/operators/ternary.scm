(declare (unit ast/operators/ternary))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-ternary-operator>
  (mop/init
    (mop/class name: "Core::AST::Operator::Ternary")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "condition")
            (attr/item "consequence")
            (attr/item "alternative"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(ternop
                   if   ,(dump (pryll:invoke self "condition"))
                   then ,(dump (pryll:invoke self "consequence"))
                   else ,(dump (pryll:invoke self "alternative"))))))
      (call finalize:))))

(define (make-ternary-operator op condition conseq alter)
  (pryll:make <pryll:ast-ternary-operator>
              location:       (token-location op)
              condition:      condition
              consequence:    conseq
              alternative:    alter))

