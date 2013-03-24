(declare (unit ast/operators/binary))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(declare (hide primop opmap))
(define (primop op type)
  (lambda (self ctx)
    (let ((left (pryll:object-data self "left"))
          (right (pryll:object-data self "right")))
      (list op
            (compile/assert-type
              type
              (compile ctx left)
              (pryll:invoke left "location"))
            (compile/assert-type
              type
              (compile ctx right)
              (pryll:invoke right "location"))))))

(define binopmap
  `(("+" ,(primop '+ type/number))
    ("-" ,(primop '- type/number))
    ("*" ,(primop '* type/number))))

(define <pryll:ast-binary-operator>
  (mop/init
    (mop/class name: "Core::AST::Operator::Binary")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "operator")
            (attr/item "left")
            (attr/item "right"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                ((cadr (assoc
                         (pryll:object-data self "operator")
                         binopmap))
                 self
                 ctx)))
            (dump-method
              (lambda (self)
                `(binop
                   ,(pryll:invoke self "operator")
                   ,(dump (pryll:invoke self "left"))
                   ,(dump (pryll:invoke self "right"))))))
      (call finalize:))))

(define (make-binary-operator op left right)
  (pryll:make <pryll:ast-binary-operator>
              location:   (token-location op)
              operator:   (token-value op)
              left:       left
              right:      right))

