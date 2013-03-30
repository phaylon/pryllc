(declare (unit ast/operators/binary))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(declare (hide primop opmap logop smartmatch))
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

(define (logop op)
  (lambda (self ctx)
    (let ((left (pryll:object-data self "left"))
          (right (pryll:object-data self "right")))
      (list op
            (compile ctx left)
            (compile ctx right)))))

(define (smartmatch self ctx)
  (let* ((left  (pryll:object-data self "left"))
         (right (pryll:object-data self "right")))
    `(pryll:invoke
       ,(compile ctx right)
       "match"
       (list ,(compile ctx left)))))

(define binopmap
  `(("+"    ,(primop '+ type/number))
    ("-"    ,(primop '- type/number))
    ("*"    ,(primop '* type/number))
    ("or"   ,(logop 'or))
    ("and"  ,(logop 'and))
    ("||"   ,(logop 'or))
    ("&&"   ,(logop 'and))
    ("~~"   ,smartmatch)
    ))

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
                (let* ((op (pryll:object-data self "operator"))
                       (item (assoc op binopmap)))
                  (if item
                    ((cadr item) self ctx)
                    (error "Unimplemented binop" op)))))
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

