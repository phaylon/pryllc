(declare (unit ast/lexical/declarations))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define-inline (compile-lexical-declarations ctx location declarations)
  (let ((vars (map (lambda (decl)
                     (let* ((lexvar (car decl))
                            (init (and (= (length decl) 2) (cadr decl)))
                            (var (compile/scoped-var
                                   (pryll:invoke lexvar "value")
                                   location)))
                       (list var
                             (pryll:invoke
                               var
                               "compile-declare"
                               (list ctx init)))))
                   declarations)))
    (for-each (lambda (item)
                (pryll:invoke
                  ctx
                  "prepare-variable"
                  (list (car item))))
              vars)
    `(begin
       ,@(map cadr vars)
       ,(pryll:invoke (caar vars) "symbol"))))

(define <pryll:ast-lexical-declarations>
  (mop/init
    (mop/class name: "Core::AST::Lexical::Declarations")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "declarations"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile-lexical-declarations
                  ctx
                  (pryll:object-data self "location")
                  (pryll:object-data self "declarations"))))
            (dump-method
              (lambda (self)
                `(my
                   ,@(map (lambda (item)
                            (if (= (length item) 1)
                              (list (dump (car item)))
                              (list (dump (car item))
                                    '=
                                    (dump (cadr item)))))
                          (pryll:object-data self "declarations"))))))
      (call finalize:))))

(define (make-lexical-declarations op declarations)
  (pryll:make <pryll:ast-lexical-declarations>
              location:     (token-location op)
              declarations: declarations))

