(declare (unit ast/lexical/variables))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define-inline (find-variable self ctx)
  (let* ((name (pryll:object-data self "value"))
         (var (pryll:invoke ctx "find-variable" (vector name))))
    (if (v-true? var)
      var
      `(pryll:err
         <pryll:error-syntax>
         location: (list ,@(pryll:object-data self "location"))
         message: (conc "Unbound lexical variable '"
                        name
                        "'")))))

(define-inline (compile-assign self ctx expr)
  (let* ((name (pryll:object-data self "value"))
         (var (find-variable self ctx)))
    `(begin
       ,(pryll:invoke var "compile-assign" (vector ctx expr))
       ,(pryll:invoke var "compile-access" (vector ctx)))))

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
                        (pryll:object-data (v1 pos) "value")
                        "")))
            (mop/method
              name: "compile-assign"
              code: (lambda (pos nam)
                      (compile-assign
                        (v1 pos)
                        (v2 pos)
                        (v3 pos))))
            (compile-method
              (lambda (self ctx)
                (let* ((name (pryll:object-data self "value"))
                       (var (pryll:invoke
                              ctx
                              "find-variable"
                              (vector name))))
                  (if var
                    (pryll:invoke var "compile-access" (vector ctx))
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

