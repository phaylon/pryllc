(declare (unit ast/splices))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-splice-hash>
  (mop/init
    (mop/class name: "Core::AST::Splice::Hash")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "expression"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(% ,(dump-slot self "expression")))))
      (call finalize:))))

(define (make-hash-splice token expr)
  (pryll:make <pryll:ast-splice-hash>
              location:   (token-location token)
              expression: expr))

(define <pryll:ast-splice-array>
  (mop/init
    (mop/class name: "Core::AST::Splice::Array")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "expression"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(@ ,(dump-slot self "expression")))))
      (call finalize:))))

(define (make-array-splice token expr)
  (pryll:make <pryll:ast-splice-array>
              location:   (token-location token)
              expression: expr))

(define (hash-splice? item)
  (pryll:isa? item <pryll:ast-splice-hash>))

(define (array-splice? item)
  (pryll:isa? item <pryll:ast-splice-array>))

