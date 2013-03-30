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
            (mop/method
              name: "name"
              code: (lambda (pos nam)
                      (dbg "namespace to name")
                      (pryll:object-data (car pos) "value")))
            (compile-method
              (lambda (self ctx)
                (let* ((name (pryll:object-data self "value"))
                       (ident (pryll:invoke ctx
                                            "find-identifier"
                                            (list name))))
                  (if (v-true? ident)
                    (let ((var (pryll:invoke ident "variable")))
                      `(force ,var))
                    (pryll:err <pryll:error-syntax>
                               location: (pryll:object-data
                                           self
                                           "location")
                               message: (conc
                                          "Unbound identifier '"
                                          name
                                          "'"))))))
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

