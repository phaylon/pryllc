(declare (unit ast))
(declare (uses util mop compiler))

(import chicken scheme)
(require-extension irregex)

(define (token-value token) (car token))
(define (token-location token) (cadr token))

(define-inline (dump item)
  (pryll:invoke
    item
    "debug-dump"))

(define-inline (dump-slot obj slot)
  (dump (pryll:object-data obj slot)))

(define-inline (dump-method proc)
  (mop/method
    name: "debug-dump"
    code: (lambda (pos nam)
            (proc (car pos)))))

(define-inline (compile-method proc)
  (mop/method
    name: "compile"
    code: (lambda (pos nam)
            (proc (car pos) (cadr pos)))))

(define-inline (compile ctx item)
  (pryll:invoke item "compile" (list ctx) (mkhash)))

(define-inline (attr/item name)
  (mop/attribute
    name: name
    init-arg: name
    reader: name))

;;
;; integer
;;

(define <pryll:ast-number>
  (mop/init
    (mop/class name: "Core::AST::Number")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (string->number (pryll:invoke self "value"))))
            (dump-method
              (lambda (self)
                `(num ,(pryll:invoke self "value")))))
      (call finalize:))))

(define (make-number token)
  (pryll:make <pryll:ast-number>
              location:   (token-location token)
              value:      (token-value token)))

;;
;; statement
;;

(define <pryll:ast-statement>
  (mop/init
    (mop/class name: "Core::AST::Statement")
    (lambda (call)
      (call add-attributes:
            (attr/item "expression"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile ctx (pryll:invoke self "expression"))))
            (dump-method
              (lambda (self)
                `(stmt ,(dump-slot self "expression")))))
      (call finalize:))))

(define (make-statement expression)
  (pryll:make <pryll:ast-statement>
              expression: expression))

;;
;; document
;;

(define <pryll:ast-document>
  (mop/init
    (mop/class name: "Core::AST::Document")
    (lambda (call)
      (call add-attributes:
            (attr/item "statements"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                `(begin
                   ,@(map
                       (lambda (stmt)
                         (compile ctx stmt))
                       (pryll:invoke self "statements")))))
            (dump-method
              (lambda (self)
                `(doc ,(map dump
                            (pryll:invoke
                              self
                              "statements"))))))
      (call finalize:))))

(define (make-document statements)
  (pryll:make <pryll:ast-document>
              statements: statements))

;;
;; binary operators
;;

(declare (hide primop opmap))

(define (primop op type)
  (lambda (self ctx)
    (let ((left (pryll:object-data self "left"))
          (right (pryll:object-data self "right")))
      (list op
            (compile/assert-type
              type
              (compile ctx left)
              (pryll:invoke left "location"))
            (compile/assert-type
              type
              (compile ctx right)
              (pryll:invoke right "location"))))))

(define binopmap
  `(("+" ,(primop '+ type/number))
    ("-" ,(primop '- type/number))
    ("*" ,(primop '* type/number))))

(define <pryll:ast-binary-operator>
  (mop/init
    (mop/class name: "Core::AST::Operator::Binary")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "operator")
            (attr/item "left")
            (attr/item "right"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                ((cadr (assoc
                         (pryll:object-data self "operator")
                         binopmap))
                 self
                 ctx)))
            (dump-method
              (lambda (self)
                `(binop
                   ,(pryll:invoke self "operator")
                   ,(dump (pryll:invoke self "left"))
                   ,(dump (pryll:invoke self "right"))))))
      (call finalize:))))

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
  (let ((unops
        `(("-" ,(lambda (self ctx operand)
                  (let ((c-operand (pryll:invoke
                                     operand
                                     "compile"
                                     (list ctx))))
                    (if (number? c-operand)
                      (* c-operand -1)
                      (pryll:err
                        <pryll:error-syntax>
                        message: (conc "Unary negation operator '-' "
                                       "(minus) can only be used on "
                                       "numbers")))))))))
    (mop/init
      (mop/class name: "Core::AST::Operator::Binary")
      (lambda (call)
        (call add-attributes:
              (attr/item "location")
              (attr/item "operator")
              (attr/item "position")
              (attr/item "operand"))
        (call add-methods:
              (compile-method
                (lambda (self ctx)
                  (let ((op-comp (cadr (assoc
                                         (pryll:invoke
                                           self
                                           "operator")
                                         unops))))
                    (op-comp
                      self
                      ctx
                      (pryll:invoke self "operand")))))
              (dump-method
                (lambda (self)
                  `(unop
                     ,(pryll:invoke self "operator")
                     ,(dump (pryll:invoke self "operand"))))))
        (call finalize:)))))

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
  (mop/init
    (mop/class name: "Core::AST::Operator::Ternary")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "condition")
            (attr/item "consequence")
            (attr/item "alternative"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(ternop
                   if   ,(dump (pryll:invoke self "condition"))
                   then ,(dump (pryll:invoke self "consequence"))
                   else ,(dump (pryll:invoke self "alternative"))))))
      (call finalize:))))

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
  (mop/init
    (mop/class name: "Core::AST::Bareword")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                (string->symbol (pryll:invoke self "value")))))
      (call finalize:))))

(define (make-bareword token)
  (pryll:make <pryll:ast-bareword>
              location:   (token-location token)
              value:      (token-value token)))

;;
;; calls
;;

(define <pryll:ast-call>
  (mop/init
    (mop/class name: "Core::AST::Call")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "function")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(call ,(dump-slot self "function")
                       ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-call op func args)
  (pryll:make <pryll:ast-call>
              location:   (token-location op)
              function:   func
              arguments:  args))

;;
;; function calls
;;

(define <pryll:ast-function-call>
  (mop/init
    (mop/class name: "Core::AST::Call::Function")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "function-name")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(func-call ,(pryll:invoke self "function-name")
                            ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-function-call name args)
  (pryll:make <pryll:ast-function-call>
              location:         (token-location name)
              function-name:    (token-value name)
              arguments:        args))

;;
;; lexical variables
;;

(define <pryll:ast-variable-lexical>
  (mop/init
    (mop/class name: "AST::Variable::Lexical")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(lex ,(string->symbol
                         (pryll:invoke self "value"))))))
      (call finalize:))))

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
  (mop/init
    (mop/class name: "Core::AST::Assign")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "target")
            (attr/item "expression"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(assign
                   ,(dump-slot self "target")
                   ,(dump-slot self "expression")))))
      (call finalize:))))

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
  (mop/init
    (mop/class name: "Core::AST::Equality")
    (lambda (call)
      (call add-attributes:
            (attr/item "items"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(equality
                   ,@(map (lambda (item)
                            (if (string? item)
                              item
                              (dump item)))
                          (pryll:invoke self "items"))))))
      (call finalize:))))

(define (make-equality-operations op left right)
  (pryll:make <pryll:ast-equality-operation>
              items: (list left (token-value op) right)))

(define (combine-equality-operations op left-eq right)
  (pryll:make <pryll:ast-equality-operation>
              items: (append
                       (pryll:invoke left-eq "items")
                       (list (token-value op) right))))

;;
;; method refs
;;

(define <pryll:ast-method-ref>
  (mop/init
    (mop/class name: "Core::AST::Ref::Method")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "is-maybe")
            (attr/item "invocant")
            (attr/item "method")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(ref-method
                   ,@(if (pryll:invoke self "is-maybe")
                       '(maybe:)
                       '())
                   ,(dump-slot self "invocant")
                   ,(dump-slot self "method")
                   ,(dump-slot self "arguments")))))
      (call finalize:))))

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
  (mop/init
    (mop/class name: "Core::AST::Call::Method")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "is-maybe")
            (attr/item "is-chained")
            (attr/item "invocant")
            (attr/item "method")
            (attr/item "arguments"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (let ((var-inv (compile/genvar 'invocant))
                      (var-met (compile/genvar 'method))
                      (inv     (pryll:object-data self "invocant"))
                      (met     (pryll:object-data self "method"))
                      (chain   (pryll:object-data self "is-chained"))
                      (maybe   (pryll:object-data self "is-maybe"))
                      (args    (pryll:object-data self "arguments")))
                  `(let ((,var-inv ,(compile ctx inv))
                         (,var-met ,(compile ctx met)))
                     (pryll:invoke
                       ,var-inv
                       ,var-met
                       ,(pryll:invoke
                          args
                          "compile-positional"
                          (list ctx))
                       ,(pryll:invoke
                          args
                          "compile-named"
                          (list ctx))
                       ,(if maybe
                          `(lambda args (void))
                          #f)
                       (list ,@(pryll:object-data self "location")))
                     ,@(if chain
                         (list var-inv)
                         '())))))
            (dump-method
              (lambda (self)
                `(call-method
                   ,@(if (pryll:invoke self "is-maybe")
                       '(maybe)
                       '())
                   ,@(if (pryll:invoke self "is-chained")
                       '(chained)
                       '())
                   ,(dump-slot self "invocant")
                   ,(dump-slot self "method")
                   ,(dump-slot self "arguments")))))
      (call finalize:))))

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
  (mop/init
    (mop/class name: "Core::AST::Named")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(named
                   ,(dump-slot self "name")
                   ,(dump-slot self "value")))))
      (call finalize:))))

(define (make-named-value op name value)
  (pryll:make <pryll:ast-named-value>
              location:   (token-location op)
              name:       name
              value:      value))

;;
;; slot ref
;;

(define-inline (compile-slot-access self ctx method extra)
  (let ((var-cont (compile/genvar 'container))
        (var-slot (compile/genvar 'slot))
        (loc (pryll:object-data self "location"))
        (cont (pryll:object-data self "container"))
        (slot (pryll:object-data self "slot")))
    `(let ((,var-cont ,(compile ctx cont))
           (,var-slot ,(compile ctx slot)))
       (pryll:invoke
         ,var-cont
         ,method
         (list ,var-slot ,@extra)))))
  
(define <pryll:ast-slot-ref>
  (mop/init
    (mop/class name: "Core::AST::SlotRef")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "container")
            (attr/item "slot"))
      (call add-methods:
            (mop/method
              name: "compile-set"
              code: (unwrap-pos-args
                      (lambda (self ctx new-value)
                        (compile-slot-access
                          self
                          ctx
                          "set"
                          (list (compile ctx new-value))))))
            (compile-method
              (lambda (self ctx)
                (compile-slot-access self ctx "get" '())))
            (dump-method
              (lambda (self)
                `(slot
                   ,(dump-slot self "container")
                   ,(dump-slot self "slot")))))
      (call finalize:))))

(define (make-slot-ref op container slot)
  (pryll:make <pryll:ast-slot-ref>
              location:   (token-location op)
              container:  container
              slot:       slot))

;;
;; positional and named
;;

(define-inline (named-value? item)
  (pryll:isa? item <pryll:ast-named-value>))

(define-inline (hash-splice? item)
  (pryll:isa? item <pryll:ast-splice-hash>))

(define-inline (array-splice? item)
  (pryll:isa? item <pryll:ast-splice-array>))

(define-inline (named-arg? item)
  (or (named-value? item)
      (hash-splice? item)))

(define-inline (positional-arg? item)
  (not (named-arg? item)))

(define-inline (compile-nam-args self ctx)
  `(alist->hash-table
     (append
       ,@(map (lambda (item)
                (if (hash-splice? item)
                  `(hash-table->alist
                     (compile/assert-type
                       type/hash
                       (compile ctx item)
                       (pryll:invoke item "location")))
                  `(list
                     (cons
                       ,(compile ctx (pryll:invoke item "name"))
                       ,(compile ctx (pryll:invoke item "value"))))))
              (filter named-arg?
                      (pryll:object-data self "items"))))))

(define-inline (compile-pos-args self ctx)
  `(append
     ,@(map (lambda (item)
              (if (array-splice? item)
                (compile/assert-type
                  type/array
                  (compile ctx item)
                  (pryll:invoke item "location"))
                (list
                  'list
                  (compile ctx item))))
            (filter positional-arg?
                    (pryll:object-data self "items")))))

;;
;; hashes
;;

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

;;
;; arrays
;;

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

;;
;; arguments
;;

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
                `(array
                   ,@(map dump
                          (pryll:invoke self "items"))))))
      (call finalize:))))

(define (make-arguments ls)
  (pryll:make <pryll:ast-arguments>
              items: ls))

;;
;; identifiers
;;

(define <pryll:ast-identifier>
  (mop/init
    (mop/class name: "Core::AST::Identifier")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(ident ,(pryll:invoke self "value")))))
      (call finalize:))))

(define (make-identifier token)
  (pryll:make <pryll:ast-identifier>
              location:   (token-location token)
              value:      (token-value token)))

;;
;; strings
;;

(define <pryll:ast-string>
  (mop/init
    (mop/class name: "Core::AST::String")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (pryll:invoke self "value")))
            (dump-method
              (lambda (self)
                `(str ,(pryll:invoke self "value")))))
      (call finalize:))))

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
              location:   (pryll:invoke ident "location")
              value:      (pryll:invoke ident "value")))

;;
;; splices
;;

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

