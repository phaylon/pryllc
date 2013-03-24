(declare (unit ast))
(declare (uses util mop compiler nether))

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
                (compile/statements
                  ctx
                  (pryll:invoke self "statements"))))
;                `(begin
;                   ,@(map
;                       (lambda (stmt)
;                         (compile ctx stmt))
;                       (pryll:invoke self "statements")))))
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
            (compile-method
              (lambda (self ctx)
                (let ((args (pryll:object-data self "arguments")))
                `(pryll:call
                   ,(compile ctx (pryll:object-data self "function"))
                   ,(pryll:invoke args "compile-positional" (list ctx))
                   ,(pryll:invoke args "compile-named" (list ctx))
                   (list ,@(pryll:object-data self "location"))))))
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
;; nether calls
;;

(define <pryll:ast-nether-call>
  (mop/init
    (mop/class name: "Core::AST::Call::Nether")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "is-chained")
            (attr/item "invocant")
            (attr/item "method")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(nether-call
                   ,@(if (pryll:invoke self "is-chained")
                       '(chained)
                       '())
                   ,(dump-slot self "invocant")
                   ,(dump-slot self "method")
                   ,(dump-slot self "arguments"))))
            (compile-method
              (lambda (self ctx)
                (let* ((method (pryll:object-data
                                 (pryll:object-data self "method")
                                 "value"))
                       (nether (nether-method method))
                       (var-inv (compile/genvar 'invocant))
                       (inv (pryll:object-data self "invocant"))
                       (args (pryll:object-data self "arguments"))
                       (chain (pryll:object-data self "is-chained"))
                       (loc (pryll:object-data self "location")))
                  (if nether
                    `(let ((,var-inv ,(compile ctx inv)))
                       (,nether
                         (append
                           (list ,var-inv)
                           ,(pryll:invoke
                              args
                              "compile-positional"
                              (list ctx)))
                         ,(pryll:invoke
                            args
                            "compile-named"
                            (list ctx)))
                       ,@(if chain
                           (list var-inv)
                           '()))
                    (pryll:err
                      <pryll:error-syntax>
                      location: loc
                      message: (sprintf "Invalid nether method ~s"
                                        method)))))))
      (call finalize:))))

(define (make-nether-call op inv met chained args)
  (pryll:make <pryll:ast-nether-call>
              location:       (token-location op)
              is-chained:     chained
              invocant:       inv
              method:         met
              arguments:      args))

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
                `(args
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

;;
;; blocks
;;

(define <pryll:ast-block>
  (mop/init
    (mop/class name: "Core::AST::Block")
    (lambda (call)
      (call add-attributes:
            (attr/item "statements"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile/statements
                  ctx
                  (pryll:object-data self "statements"))))
            (dump-method
              (lambda (self)
                `(block
                   ,@(map dump (pryll:object-data self "statements"))))))
      (call finalize:))))

(define (make-block statements)
  (pryll:make <pryll:ast-block>
              statements: statements))

;;
;; namespaces
;;

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

;;
;; signatures
;;

(define <pryll:ast-signature-param-pos>
  (mop/init
    (mop/class name: "Core::AST::Signature::Parameter::Positional")
    (lambda (call)
      (call add-attributes:
            (attr/item "type")
            (attr/item "variable")
            (attr/item "is-optional")
            (attr/item "default"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(positional
                   ,(dump-slot self "variable")
                   ,(let ((type (pryll:object-data self "type")))
                      (if type
                        `(type ,(dump type))
                        'no-type))
                   ,(let ((default (pryll:object-data self "default")))
                      (if default
                        `(default ,(dump default))
                        'no-default))
                   ,(let ((opt (pryll:object-data self "is-optional")))
                      (if opt 'is-optional 'is-required))))))
      (call finalize:))))

(define (make-signature-param-pos type var optional default)
  (pryll:make <pryll:ast-signature-param-pos>
              type:         type
              variable:     var
              is-optional:  (or default optional)
              default:      default))

(define <pryll:ast-signature-param-nam>
  (mop/init
    (mop/class name: "Core::AST::Signature::Parameter::Named")
    (lambda (call)
      (call add-attributes:
            (attr/item "type")
            (attr/item "variable")
            (attr/item "is-optional")
            (attr/item "default"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(named
                   ,(dump-slot self "variable")
                   ,(let ((type (pryll:object-data self "type")))
                      (if type
                        `(type ,(dump type))
                        'no-type))
                   ,(let ((default (pryll:object-data self "default")))
                      (if default
                        `(default ,(dump default))
                        'no-default))
                   ,(let ((opt (pryll:object-data self "is-optional")))
                      (if opt 'is-optional 'is-required))))))
      (call finalize:))))

(define (make-signature-param-nam type var optional default)
  (pryll:make <pryll:ast-signature-param-nam>
              type:         type
              variable:     var
              is-optional:  (or default optional)
              default:      default))

(define <pryll:ast-signature-rest-pos>
  (mop/init
    (mop/class name: "Core::AST::Signature::Rest::Positional")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "variable"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(positional-rest
                   ,(dump-slot self "variable")))))
      (call finalize:))))

(define (make-signature-rest-pos token variable)
  (pryll:make <pryll:ast-signature-rest-pos>
              location: (token-location token)
              variable: variable))

(define <pryll:ast-signature-rest-nam>
  (mop/init
    (mop/class name: "Core::AST::Signature::Rest::Named")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "variable"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(named-rest
                   ,(dump-slot self "variable")))))
      (call finalize:))))

(define (make-signature-rest-nam token variable)
  (pryll:make <pryll:ast-signature-rest-nam>
              location: (token-location token)
              variable: variable))

(define-inline (positional-rest? value)
  (pryll:isa? value <pryll:ast-signature-rest-pos>))

(define-inline (named-rest? value)
  (pryll:isa? value <pryll:ast-signature-rest-nam>))

(define-inline (named-param? value)
  (pryll:isa? value <pryll:ast-signature-param-nam>))

(define-inline (positional-param? value)
  (pryll:isa? value <pryll:ast-signature-param-pos>))

(define-inline (max-positionals self)
  (call/cc
    (lambda (cont)
      (length
        (filter (lambda (item)
                  (if (positional-param? item)
                    #t
                    (if (positional-rest? item)
                      (cont #f)
                      #f)))
                (pryll:object-data self "parameters"))))))

(define-inline (min-positionals self)
  (length
    (filter (lambda (item)
              (if (positional-param? item)
                (if (pryll:invoke item "is-optional")
                  #f
                  #t)
                #f))
            (pryll:object-data self "parameters"))))

(define-inline (named-required self)
  (map (lambda (item)
         (pryll:invoke
           (pryll:invoke item "variable")
           "identifier"))
       (filter (lambda (item)
                 (and (named-param? item)
                      (not (pryll:invoke item "is-optional"))))
               (pryll:object-data self "parameters"))))

(define-inline (signature-declare-pos self ctx src-var)
  (define (compile-pos left index)
    (if (null? left)
      '()
      (let* ((param (car left))
             (default (pryll:invoke param "default"))
             (var (compile/scoped-var
                    (pryll:invoke
                      (pryll:invoke param "variable")
                      "value")))
             (spec (list
                     (pryll:invoke var "symbol")
                     `(if (> ,index (- (length ,src-var) 1))
                        ,(if default
                           (compile ctx default)
                           '(void))
                        (list-ref ,src-var ,index)))))
        (pryll:invoke ctx "add-variable" (list var))
        (cons spec
              (compile-pos (cdr left) (+ index 1))))))
  (let* ((params (pryll:object-data self "parameters"))
         (pos (filter positional-param? params)))
    ;; TODO sanity checks
    (append
      (compile-pos pos 0))))

(define-inline (signature-declare-nam self ctx src-var)
  (let* ((params (pryll:object-data self "parameters"))
         (nam (filter named-param? params)))
    (map (lambda (param)
           (let* ((default (pryll:invoke param "default"))
                  (name (pryll:invoke
                          (pryll:invoke param "variable")
                          "identifier"))
                  (var (compile/scoped-var
                         (pryll:invoke
                           (pryll:invoke param "variable")
                           "value")))
                  (spec (list
                          (pryll:invoke var "symbol")
                          `(if (hash-table-exists? ,src-var ,name)
                             (hash-table-ref ,src-var ,name)
                             ,(if default
                                (compile ctx default)
                                '(void))))))
             (pryll:invoke ctx "add-variable" (list var))
             spec))
         nam)))

(define-inline (signature-declare-rest-pos self ctx src-var)
  (let* ((params (pryll:object-data self "parameters"))
         (pos-cnt (length (filter positional-param? params)))
         (rest (filter positional-rest? params)))
    (if (= 0 (length rest))
      (list)
      (let* ((rest-param (car rest))
             (lexvar (pryll:invoke rest-param "variable"))
             (var (compile/scoped-var
                    (pryll:invoke lexvar "value"))))
        (pryll:invoke ctx "add-variable" (list var))
        (list
          (list (pryll:invoke var "symbol")
                `(if (> (length ,src-var) ,pos-cnt)
                   (list-tail ,src-var ,pos-cnt)
                   (list))))))))

(define-inline (signature-declare-rest-nam self ctx src-var)
  (let* ((params (pryll:object-data self "parameters"))
         (known (map (lambda (param)
                       (pryll:invoke
                         (pryll:invoke param "variable")
                         "identifier"))
                     (filter named-param? params)))
         (rest (filter named-rest? params)))
    (if (= 0 (length rest))
      (list)
      (let* ((rest-param (car rest))
             (lexvar (pryll:invoke rest-param "variable"))
             (var-pair (compile/genvar 'pair))
             (var (compile/scoped-var
                    (pryll:invoke lexvar "value"))))
        (pryll:invoke ctx "add-variable" (list var))
        (list
          (list (pryll:invoke var "symbol")
                `(alist->hash-table
                   (filter (lambda (,var-pair)
                             (not (contains-str
                                    (list ,@known)
                                    (car ,var-pair))))
                           (hash-table->alist ,src-var)))))))))

(define-inline (all-named self)
  (map (lambda (param)
         (pryll:invoke (pryll:invoke param "variable") "identifier"))
       (filter named-param? (pryll:invoke self "parameters"))))

(define-inline (has-named-rest self)
  (< 0 (length (filter named-rest? (pryll:invoke self "parameters")))))

(define (compile-signature-scope self ctx var-pos var-nam block)
  (let ((var-key (compile/genvar 'key))
        (var-unknown (compile/genvar 'missing))
        (pos-min (min-positionals self))
        (pos-max (max-positionals self))
        (nam-req (named-required self))
        (nam-known (all-named self))
        (nam-rest (has-named-rest self))
        (subctx  (subcontext ctx)))
    `(begin
       ,@(if (and pos-min (> pos-min 0))
           `((if (< (length ,var-pos) ,pos-min)
               (pryll:err
                 <pryll:error-arguments>
                 message:
                 (conc
                   ,(conc
                      "Expected at least "
                      pos-min
                      " positional "
                      (if (= pos-min 1) "argument" "arguments")
                      ", but only received ")
                   (length ,var-pos)))))
           '())
       ,@(if pos-max
           `((if (> (length ,var-pos) ,pos-max)
               (pryll:err
                 <pryll:error-arguments>
                 message:
                 (conc
                   ,(conc
                      "Expected at most "
                      pos-max
                      " positional "
                      (if (= pos-max 1) "argument" "arguments")
                      ", but received ")
                   (length ,var-pos)))))
           '())
       ,@(if (> (length nam-req) 0)
           `((for-each (lambda (,var-key)
                         (if (not (hash-table-exists? ,var-nam ,var-key))
                           (error "Missing named argument" ,var-key)))
                       (list ,@nam-req)))
           '())
       ,@(if nam-rest
           '()
           `((let ((,var-unknown (str-unknown
                                   (list ,@nam-known)
                                   (hash-table-keys ,var-nam))))
               (if (< 0 (length ,var-unknown))
                 (error "Unknown named args" ,var-unknown)))))
       (let ,(append
               (signature-declare-pos self subctx var-pos)
               (signature-declare-nam self subctx var-nam)
               (signature-declare-rest-pos self subctx var-pos)
               (signature-declare-rest-nam self subctx var-nam))
         ,(compile subctx block)))))

(define <pryll:ast-signature>
  (mop/init
    (mop/class name: "Core::AST::Signature")
    (lambda (call)
      (call add-attributes:
            (attr/item "parameters"))
      (call add-methods:
            (mop/method
              name: "compile-scope"
              code: (lambda (pos nam)
                      (apply compile-signature-scope pos)))
            (dump-method
              (lambda (self)
                `(signature
                   ,@(map dump (pryll:object-data self "parameters"))))))
      (call finalize:))))

(define (make-signature parameters)
  (pryll:make <pryll:ast-signature>
              parameters: parameters))

;;
;; lambdas
;;

(define-inline (compile-lambda ctx location signature block)
  (let ((var-pos (compile/genvar 'pos))
        (var-nam (compile/genvar 'nam))
        (var-none (compile/genvar 'none)))
    (if signature
      `(lambda (,var-pos ,var-nam)
         (void)
         ,(pryll:invoke
            signature
            "compile-scope"
            (list ctx var-pos var-nam block)))
      (let ((subctx (subcontext ctx)))
        `(lambda ,var-none
           (void)
           ,(compile subctx block))))))         

(define <pryll:ast-lambda>
  (mop/init
    (mop/class name: "Core::AST::Lambda")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "signature")
            (attr/item "block"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (compile-lambda
                  ctx
                  (pryll:object-data self "location")
                  (pryll:object-data self "signature")
                  (pryll:object-data self "block"))))
            (dump-method
              (lambda (self)
                `(lambda
                   ,(let ((signature
                            (pryll:object-data self "signature")))
                      (if signature
                        (dump signature)
                        'no-signature))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-lambda token signature block)
  (pryll:make <pryll:ast-lambda>
              location:  (token-location token)
              signature: signature
              block:     block))

;;
;; lexical declarations
;;

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

;;
;; modules
;;

(define <pryll:ast-module>
  (mop/init
    (mop/class name: "Core::AST::Module")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "traits")
            (attr/item "block"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(module
                   ,(dump-slot self "name")
                   ,@(map dump (pryll:object-data self "traits"))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-module op name traits block)
  (pryll:make <pryll:ast-module>
              location: (token-location op)
              traits:   traits
              name:     name
              block:    block))

;;
;; traits
;;

(define <pryll:ast-trait>
  (mop/init
    (mop/class name: "Core::AST::Trait")
    (lambda (call)
      (call add-attributes:
            (attr/item "name")
            (attr/item "arguments"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(trait
                   ,(dump-slot self "name")
                   ,(dump-slot self "arguments")))))
      (call finalize:))))

(define (make-trait name arguments)
  (pryll:make <pryll:ast-trait>
              name:         name
              arguments:    arguments))

;;
;; functions
;;

(define <pryll:ast-function>
  (mop/init
    (mop/class name: "Core::AST::Function")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "signature")
            (attr/item "traits")
            (attr/item "block"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(function
                   ,(pryll:object-data self "name")
                   ,(let ((s (pryll:object-data self "signature")))
                      (if s (dump s) 'no-signature))
                   ,@(map dump (pryll:object-data self "traits"))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-function op name signature traits block)
  (pryll:make <pryll:ast-function>
              location:     (token-location op)
              name:         (pryll:invoke name "value")
              signature:    signature
              traits:       traits
              block:        block))

;;
;; subroutines
;;

(define <pryll:ast-subroutine>
  (mop/init
    (mop/class name: "Core::AST::Subroutine")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "signature")
            (attr/item "traits")
            (attr/item "block"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(sub
                   ,(pryll:object-data self "name")
                   ,(let ((s (pryll:object-data self "signature")))
                      (if s (dump s) 'no-signature))
                   ,@(map dump (pryll:object-data self "traits"))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-subroutine op name signature traits block)
  (pryll:make <pryll:ast-subroutine>
              location:     (token-location op)
              name:         (pryll:invoke name "value")
              signature:    signature
              traits:       traits
              block:        block))

;;
;; conditions
;;

(define <pryll:ast-condition>
  (mop/init
    (mop/class name: "Core::AST::Condition")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "type")
            (attr/item "condition")
            (attr/item "block")
            (attr/item "else"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(,(string->symbol (pryll:object-data self "type"))
                   ,(dump-slot self "condition")
                   (then ,(dump-slot self "block"))
                   ,@(let ((e (pryll:object-data self "else")))
                       (if e `((else ,(dump e))) '()))))))
      (call finalize:))))

(define (make-condition type condition block tail)
  (pryll:make <pryll:ast-condition>
              location:     (token-location type)
              type:         (token-value type)
              condition:    condition
              block:        block
              else:         tail))
              
(define <pryll:ast-return>
  (mop/init
    (mop/class name: "Core::AST::Return")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "expression"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(return ,(dump-slot self "expression")))))
      (call finalize:))))

(define (make-return op expr)
  (pryll:make <pryll:ast-return>
              location:     (token-location op)
              expression:   expr))


