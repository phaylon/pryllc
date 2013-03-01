(load "lib/objects.scm")
(load "lib/util.scm")

(module pryll/ast
  (<pryll:ast-number>
   <pryll:ast-statement>
   <pryll:ast-document>
   <pryll:ast-binary-operator>
   <pryll:ast-unary-operator>
   <pryll:ast-ternary-operator>
   <pryll:ast-bareword>
   <pryll:ast-call>
   <pryll:ast-function-call>
   <pryll:ast-variable-lexical>
   <pryll:ast-assign>
   <pryll:ast-equality-operation>
   <pryll:ast-method-ref>
   <pryll:ast-method-call>
   <pryll:ast-named-value>
   <pryll:ast-slot-ref>
   <pryll:ast-hash>
   <pryll:ast-array>
   <pryll:ast-arguments>
   <pryll:ast-identifier>
   <pryll:ast-string>
   <pryll:ast-splice-hash>
   <pryll:ast-splice-array>
   make-number
   make-statement
   make-document
   make-binary-operator
   make-unary-operator
   make-ternary-operator
   make-bareword
   make-call
   make-function-call
   make-lexical-variable
   make-assign
   make-assign/sc
   make-equality-operations
   combine-equality-operations
   make-method-ref
   make-method-call
   make-named-value
   make-slot-ref
   make-hash
   make-array
   make-arguments
   make-identifier
   make-string-value
   make-string-single
   make-string-double
   identifier->string
   make-hash-splice
   make-array-splice)
  (import chicken scheme)
  (import pryll/objects pryll/util)
  (require-extension irregex)

  (define (token-value token) (car token))
  (define (token-location token) (cadr token))

;;
;; integer
;;

  (define (dump item)
    (pryll:call-method
      item
      "debug-dump"
      (list)
      (mkhash)))

  (define (dump-slot obj slot)
    (dump (pryll:get-slot obj slot)))

  (define (dump-method proc)
    (pryll:mkmethod
      "debug-dump"
      (lambda (pos nam)
        (proc (car pos)))))

  (define (compile-method proc)
    (pryll:mkmethod
      "compile"
      (lambda (pos nam)
        (proc (car pos) (cadr pos)))))

  (define (compile ctx item)
    (pryll:call-method item "compile" (list ctx) (mkhash)))

  (define <pryll:ast-number>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "value"))
      methods: (named->hash
                 (compile-method
                   (lambda (self ctx)
                     (string->number (pryll:get-slot self "value"))))
                 (dump-method
                   (lambda (self)
                     `(num ,(pryll:get-slot self "value")))))))

  (define (make-number token)
    (pryll:make <pryll:ast-number>
                location:   (token-location token)
                value:      (token-value token)))

;;
;; statement
;;

  (define <pryll:ast-statement>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "expression"))
      methods: (named->hash
                 (compile-method
                   (lambda (self ctx)
                     (compile ctx (pryll:get-slot self "expression"))))
                 (dump-method
                   (lambda (self)
                     `(stmt ,(dump-slot self "expression")))))))

  (define (make-statement expression)
    (pryll:make <pryll:ast-statement>
                expression: expression))

;;
;; document
;;

  (define <pryll:ast-document>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "statements"))
      methods: (named->hash
                 (compile-method
                   (lambda (self ctx)
                     `(begin
                        ,@(map
                            (lambda (stmt)
                              (compile ctx stmt))
                            (pryll:get-slot self "statements")))))
                 (dump-method
                   (lambda (self)
                     `(doc ,(map dump
                                 (pryll:get-slot
                                   self
                                   "statements"))))))))

  (define (make-document statements)
    (pryll:make <pryll:ast-document>
                statements: statements))

;;
;; binary operators
;;

  (define <pryll:ast-binary-operator>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "operator")
                    (pryll:attribute/item "left")
                    (pryll:attribute/item "right"))
      methods: (named->hash
                 (pryll:mkmethod
                   "debug-dump"
                   (lambda (pos nam)
                     (let ((self (car pos)))
                       `(binop
                          ,(pryll:get-slot self "operator")
                          ,(dump (pryll:get-slot self "left"))
                          ,(dump (pryll:get-slot self "right")))))))))

  (define (make-binary-operator op left right)
    (pryll:make <pryll:ast-binary-operator>
                location:   (token-location op)
                operator:   (token-value op)
                left:       left
                right:      right))

;;
;; unary operators
;;

  (define <pryll:ast-unary-operator>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "operator")
                    (pryll:attribute/item "position")
                    (pryll:attribute/item "operand"))
      methods: (named->hash
                 (pryll:mkmethod
                   "debug-dump"
                   (lambda (pos nam)
                     (let ((self (car pos)))
                       `(unop
                          ,(pryll:get-slot self "operator")
                          ,(dump
                             (pryll:get-slot
                               self
                               "operand")))))))))

  (define (make-unary-operator op operand pos)
    (pryll:make <pryll:ast-unary-operator>
                location:   (token-location op)
                operator:   (token-value op)
                operand:    operand
                position:   pos))

;;
;; ternary operator
;;

  (define <pryll:ast-ternary-operator>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "condition")
                    (pryll:attribute/item "consequence")
                    (pryll:attribute/item "alternative"))
      methods: (named->hash
                 (pryll:mkmethod
                   "debug-dump"
                   (lambda (pos nam)
                     (let ((self (car pos)))
                       `(ternop
                          if    ,(dump
                                   (pryll:get-slot
                                     self
                                     "condition"))
                          then  ,(dump
                                   (pryll:get-slot
                                     self
                                     "consequence"))
                          else  ,(dump
                                   (pryll:get-slot
                                     self
                                     "alternative")))))))))

  (define (make-ternary-operator op condition conseq alter)
    (pryll:make <pryll:ast-ternary-operator>
                location:       (token-location op)
                condition:      condition
                consequence:    conseq
                alternative:    alter))

;;
;; barewords
;;

  (define <pryll:ast-bareword>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "value"))
      methods: (named->hash
                 (pryll:mkmethod
                   "debug-dump"
                   (lambda (pos nam)
                     (let ((self (car pos)))
                       (string->symbol
                         (pryll:get-slot self "value"))))))))

  (define (make-bareword token)
    (pryll:make <pryll:ast-bareword>
                location:   (token-location token)
                value:      (token-value token)))

;;
;; calls
;;

  (define <pryll:ast-call>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "function")
                    (pryll:attribute/item "arguments"))
      methods: (named->hash
                 (pryll:mkmethod
                   "debug-dump"
                   (lambda (pos nam)
                     (let ((self (car pos)))
                       `(call
                          ,(dump-slot self "function")
                          ,(dump-slot self "arguments"))))))))

  (define (make-call op func args)
    (pryll:make <pryll:ast-call>
                location:   (token-location op)
                function:   func
                arguments:  args))

;;
;; function calls
;;

  (define <pryll:ast-function-call>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "function-name")
                    (pryll:attribute/item "arguments"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(func-call
                        ,(pryll:get-slot self "function-name")
                        ,(dump-slot self "arguments")))))))

  (define (make-function-call name args)
    (pryll:make <pryll:ast-function-call>
                location:         (token-location name)
                function-name:    (token-value name)
                arguments:        args))

;;
;; lexical variables
;;

  (define <pryll:ast-variable-lexical>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "value"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(lex ,(string->symbol
                              (pryll:get-slot self "value"))))))))

  (define (make-lexical-variable token)
    (pryll:make <pryll:ast-variable-lexical>
                location:   (token-location token)
                value:      (token-value token)))

;;
;; assignments
;;

  (define assign-expand
    '(("+="  "+")
      ("-="  "-")
      ("*="  "*")
      ("/="  "/")
      ("~="  "~")
      ("//=" "//")
      ("||=" "||")
      ("&&=" "&&")))

  (define <pryll:ast-assign>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "target")
                    (pryll:attribute/item "expression"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(assign
                        ,(dump-slot self "target")
                        ,(dump-slot self "expression")))))))

  (define (make-assign op target expr)
    (pryll:make <pryll:ast-assign>
                location:   (token-location op)
                target:     target
                expression: expr))

  (define (make-assign/sc op target expr)
    (pryll:make <pryll:ast-assign>
                location:   (token-location op)
                target:     target
                expression: (pryll:make
                              <pryll:ast-binary-operator>
                              location: (token-location op)
                              operator: (cadr (assoc
                                                (token-value op)
                                                assign-expand))
                              left:     target
                              right:    expr)))

;;
;; equalities
;;

  (define <pryll:ast-equality-operation>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "items"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(equality
                        ,@(map (lambda (item)
                                 (if (string? item)
                                   item
                                   (dump item)))
                               (pryll:get-slot self "items"))))))))

  (define (make-equality-operations op left right)
    (pryll:make <pryll:ast-equality-operation>
                items: (list left (token-value op) right)))

  (define (combine-equality-operations op left-eq right)
    (pryll:make <pryll:ast-equality-operation>
                items: (append
                         (pryll:get-slot left-eq "items")
                         (list (token-value op) right))))

;;
;; method refs
;;

  (define <pryll:ast-method-ref>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "is-maybe")
                    (pryll:attribute/item "invocant")
                    (pryll:attribute/item "method")
                    (pryll:attribute/item "arguments"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(ref-method
                        ,@(if (pryll:get-slot self "is-maybe")
                            '(maybe:)
                            '())
                        ,(dump-slot self "invocant")
                        ,(dump-slot self "method")
                        ,(dump-slot self "arguments")))))))

  (define (make-method-ref op inv met maybe args)
    (pryll:make <pryll:ast-method-ref>
                location:   (token-location op)
                is-maybe:   maybe
                invocant:   inv
                method:     met
                arguments:  args))

;;
;; method call
;;

  (define <pryll:ast-method-call>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "is-maybe")
                    (pryll:attribute/item "is-chained")
                    (pryll:attribute/item "invocant")
                    (pryll:attribute/item "method")
                    (pryll:attribute/item "arguments"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(call-method
                        ,@(if (pryll:get-slot self "is-maybe")
                            '(maybe:)
                            '())
                        ,@(if (pryll:get-slot self "is-chained")
                            '(chained:)
                            '())
                        ,(dump-slot self "invocant")
                        ,(dump-slot self "method")
                        ,(dump-slot self "arguments")))))))

  (define (make-method-call op inv met maybe chained args)
    (pryll:make <pryll:ast-method-call>
                location:       (token-location op)
                is-maybe:       maybe
                is-chained:     chained
                invocant:       inv
                method:         met
                arguments:      args))

;;
;; named values
;;

  (define <pryll:ast-named-value>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "name")
                    (pryll:attribute/item "value"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(named
                        ,(dump-slot self "name")
                        ,(dump-slot self "value")))))))

  (define (make-named-value op name value)
    (pryll:make <pryll:ast-named-value>
                location:   (token-location op)
                name:       name
                value:      value))

;;
;; slot ref
;;

  (define <pryll:ast-slot-ref>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "container")
                    (pryll:attribute/item "slot"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(slot
                        ,(dump-slot self "container")
                        ,(dump-slot self "slot")))))))

  (define (make-slot-ref op container slot)
    (pryll:make <pryll:ast-slot-ref>
                location:   (token-location op)
                container:  container
                slot:       slot))

;;
;; hashes
;;

  (define <pryll:ast-hash>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "items"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(hash
                        ,@(map dump
                               (pryll:get-slot self "items"))))))))

  (define (make-hash op items)
    (pryll:make <pryll:ast-hash>
                location:   (token-location op)
                items:      items))

;;
;; arrays
;;

  (define <pryll:ast-array>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "items"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(array
                        ,@(map dump
                               (pryll:get-slot self "items"))))))))

  (define (make-array op items)
    (pryll:make <pryll:ast-array>
                location:   (token-location op)
                items:      items))

;;
;; arguments
;;

  (define <pryll:ast-arguments>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "items"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(args
                        ,@(map dump
                               (pryll:get-slot self "items"))))))))

  (define (make-arguments ls)
    (pryll:make <pryll:ast-arguments>
                items: ls))

;;
;; identifiers
;;

  (define <pryll:ast-identifier>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "value"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(ident ,(pryll:get-slot self "value")))))))

  (define (make-identifier token)
    (pryll:make <pryll:ast-identifier>
                location:   (token-location token)
                value:      (token-value token)))

;;
;; strings
;;

  (define <pryll:ast-string>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "value"))
      methods: (named->hash
                 (compile-method
                   (lambda (self ctx)
                     (pryll:get-slot self "value")))
                 (dump-method
                   (lambda (self)
                     `(str ,(pryll:get-slot self "value")))))))

  (define (trim-string str chr)
    (irregex-replace
      `(: bos ,chr)
      (irregex-replace
        `(: ,chr eos)
        str
        "")
      ""))

  (define (make-string-value token)
    (pryll:make <pryll:ast-string>
                location:   (token-location token)
                value:      (token-value token)))

  (define (make-string-single token)
    (pryll:make <pryll:ast-string>
                location:   (token-location token)
                value:      (irregex-replace/all
                              '(: "\\'")
                              (trim-string (token-value token) "'")
                              "'")))

  (define (prepare-string-double-chars str)
    (define replace-map
      '(("n"  "\n")
        ("t"  "\t")
        ("\"" "\"")))
    (irregex-replace/all
      '(: #\\ ($ any))
      str
      (lambda (match)
        (let ((p (assoc (irregex-match-substring match 1) replace-map)))
          (if p
            (cadr p)
            (error "Invalid escaped char"))))))

  (define (make-string-double token)
    (pryll:make <pryll:ast-string>
                location:   (token-location token)
                value:      (prepare-string-double-chars
                              (trim-string (token-value token) "\""))))

  (define (identifier->string ident)
    (pryll:make <pryll:ast-string>
                location:   (pryll:get-slot ident "location")
                value:      (pryll:get-slot ident "value")))

;;
;; splices
;;

  (define <pryll:ast-splice-hash>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "expression"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(% ,(dump-slot self "expression")))))))

  (define (make-hash-splice token expr)
    (pryll:make <pryll:ast-splice-hash>
                location:   (token-location token)
                expression: expr))

  (define <pryll:ast-splice-array>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "location")
                    (pryll:attribute/item "expression"))
      methods: (named->hash
                 (dump-method
                   (lambda (self)
                     `(@ ,(dump-slot self "expression")))))))

  (define (make-array-splice token expr)
    (pryll:make <pryll:ast-splice-array>
                location:   (token-location token)
                expression: expr))

)
