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

