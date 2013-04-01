(declare (unit ast/assign))
(declare (uses ast/util mop compiler ast/operators/binary))

(import chicken scheme)

(declare (hide assign-expand))
(define assign-expand
  '(("+="  "+")
    ("-="  "-")
    ("*="  "*")
    ("/="  "/")
    ("~="  "~")
    ("//=" "//")
    ("||=" "||")
    ("&&=" "&&")))

(define-inline (compile-assign self ctx)
  (let ((target (pryll:object-data self "target"))
        (expr (pryll:object-data self "expression")))
    (pryll:invoke target "compile-assign" (vector ctx expr))))

(define <pryll:ast-assign>
  (mop/init
    (mop/class name: "Core::AST::Assign")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "target")
            (attr/item "expression"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile-assign self ctx)))
            (dump-method
              (lambda (self)
                `(assign
                   ,(dump-slot self "target")
                   ,(dump-slot self "expression")))))
      (call finalize:))))

(define (make-assign op target expr)
  (pryll:make <pryll:ast-assign>
              location:   (token-location op)
              target:     target
              expression: expr))

(define (make-assign/sc op target expr)
  (pryll:make <pryll:ast-assign>
              location:   (token-location op)
              target:     target
              expression: (pryll:make
                            <pryll:ast-binary-operator>
                            location: (token-location op)
                            operator: (cadr (assoc
                                              (token-value op)
                                              assign-expand))
                            left:     target
                            right:    expr)))

