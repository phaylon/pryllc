(declare (unit ast/hashes))
(declare (uses ast/util mop compiler ast/arguments))

(import chicken scheme)

(define <pryll:ast-hash>
  (mop/init
    (mop/class name: "Core::AST::Hash")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "items"))
      (call add-methods:
            (compile-method compile-nam-args)
            (dump-method
              (lambda (self)
                `(hash
                   ,@(map dump
                          (pryll:invoke self "items"))))))
      (call finalize:))))

(define (make-hash op items)
  (pryll:make <pryll:ast-hash>
              location:   (token-location op)
              items:      items))

