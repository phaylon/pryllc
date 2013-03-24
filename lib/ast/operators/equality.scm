(declare (unit ast/operators/equality))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-equality-operation>
  (mop/init
    (mop/class name: "Core::AST::Equality")
    (lambda (call)
      (call add-attributes:
            (attr/item "items"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(equality
                   ,@(map (lambda (item)
                            (if (string? item)
                              item
                              (dump item)))
                          (pryll:invoke self "items"))))))
      (call finalize:))))

(define (make-equality-operations op left right)
  (pryll:make <pryll:ast-equality-operation>
              items: (list left (token-value op) right)))

(define (combine-equality-operations op left-eq right)
  (pryll:make <pryll:ast-equality-operation>
              items: (append
                       (pryll:invoke left-eq "items")
                       (list (token-value op) right))))

