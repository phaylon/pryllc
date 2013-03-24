(declare (unit ast/namespaces))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-namespace>
  (mop/init
    (mop/class name: "Core::AST::Namespace")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(ns ,(pryll:object-data self "value")))))
      (call finalize:))))

(define (make-namespace elements)
  (pryll:make <pryll:ast-namespace>
              location: (token-location (car elements))
              value:    (string-join
                          (map token-value elements)
                          "::")))

