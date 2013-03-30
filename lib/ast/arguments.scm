(declare (unit ast/arguments))
(declare (uses ast/util mop compiler ast/splices))

(import chicken scheme)

(define (named-arg? item)
  (or (named-value? item)
      (hash-splice? item)))

(define (positional-arg? item)
  (not (named-arg? item)))

(define (compile-nam-args self ctx)
  `(alist->hash-table
     (append
       ,@(map (lambda (item)
                (if (hash-splice? item)
                  `(hash-table->alist
                     (compile/assert-type
                       type/hash
                       (compile ctx (pryll:invoke item "expression"))
                       (pryll:invoke item "location")))
                  `(list
                     (cons
                       ,(compile ctx (pryll:invoke item "name"))
                       ,(compile ctx (pryll:invoke item "value"))))))
              (filter named-arg?
                      (pryll:object-data self "items"))))))

(define (compile-pos-args self ctx)
  `(append
     ,@(map (lambda (item)
              (dbg "pos arg")
              (if (array-splice? item)
                (compile/assert-type
                  type/array
                  (compile ctx (pryll:invoke item "expression"))
                  (pryll:invoke item "location"))
                (list
                  'list
                  (compile ctx item))))
            (filter positional-arg?
                    (pryll:object-data self "items")))))

(define <pryll:ast-arguments>
  (mop/init
    (mop/class name: "Core::AST::Arguments")
    (lambda (call)
      (call add-attributes:
            (attr/item "items"))
      (call add-methods:
            (mop/method
              name: "compile-positional"
              code: (unwrap-pos-args compile-pos-args))
            (mop/method
              name: "compile-named"
              code: (unwrap-pos-args compile-nam-args))
            (dump-method
              (lambda (self)
                `(args
                   ,@(map dump
                          (pryll:invoke self "items"))))))
      (call finalize:))))

(define (make-arguments ls)
  (pryll:make <pryll:ast-arguments>
              items: ls))

