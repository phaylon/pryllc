(declare (unit ast/lexical/variables))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-variable-lexical>
  (mop/init
    (mop/class name: "AST::Variable::Lexical")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (mop/method
              name: "identifier"
              code: (lambda (pos nam)
                      (irregex-replace
                        '(: bos "$")
                        (pryll:object-data (car pos) "value")
                        "")))
            (compile-method
              (lambda (self ctx)
                (let* ((name (pryll:object-data self "value"))
                       (var (pryll:invoke
                              ctx
                              "find-variable"
                              (list name))))
                  (if var
                    (pryll:invoke var "compile-access" (list ctx))
                    (error "Unbound lexical variable" name)))))
            (dump-method
              (lambda (self)
                `(lex ,(string->symbol
                         (pryll:invoke self "value"))))))
      (call finalize:))))

(define (make-lexical-variable token)
  (pryll:make <pryll:ast-variable-lexical>
              location:   (token-location token)
              value:      (token-value token)))

