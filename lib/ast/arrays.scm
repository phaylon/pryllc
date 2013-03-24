(declare (unit ast/arrays))
(declare (uses ast/util mop compiler ast/arguments))

(import chicken scheme)

(define <pryll:ast-array>
  (mop/init
    (mop/class name: "Core::AST::Array")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "items"))
      (call add-methods:
            (compile-method compile-pos-args)
            (dump-method
              (lambda (self)
                `(array
                   ,@(map dump
                          (pryll:invoke self "items"))))))
      (call finalize:))))

(define (make-array op items)
  (pryll:make <pryll:ast-array>
              location:   (token-location op)
              items:      items))

