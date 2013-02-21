
(module ast *
  (import chicken scheme)
  (require-extension coops)

  (define (token-value token) (car token))
  (define (token-location token) (cadr token))

;;
;; integer
;;

  (define-class <ast-number> ()
    ((location)
     (value)))

  (define-method (debug-dump (num <ast-number>))
    `(num ,(slot-value num 'value)))

;;
;; statement
;;

  (define-class <ast-statement> ()
    ((expression reader: expression)))

  (define-method (debug-dump (stmt <ast-statement>))
    `(stmt ,(debug-dump (slot-value stmt 'expression))))

;;
;; document
;;

  (define-class <ast-document> ()
    ((statements reader: statements)))

  (define-method (debug-dump (doc <ast-document>))
    `(doc ,(map debug-dump (slot-value doc 'statements))))

;;
;; binary operators
;;

  (define-class <ast-binary-operator> ()
    ((location)
     (operator reader: operator)
     (left     reader: left-operand)
     (right    reader: right-operand)))

  (define-method (debug-dump (op <ast-binary-operator>))
    `(binop ,(operator op)
            ,(debug-dump (left-operand op))
            ,(debug-dump (right-operand op))))

;;
;; unary operators
;;

  (define-class <ast-unary-operator> ()
    ((location)
     (operator reader: operator)
     (position reader: operator-position)
     (operand  reader: operand)))

  (define-method (debug-dump (op <ast-unary-operator>))
    `(unop ,(operator op) ,(debug-dump (operand op))))

;;
;; ternary operator
;;

  (define-class <ast-ternary-operator> ()
    ((location)
     (condition   reader: condition)
     (consequence reader: consequence)
     (alternative reader: alternative)))

  (define-method (debug-dump (tern <ast-ternary-operator>))
    `(ternop
      if:   ,(debug-dump (condition tern))
      then: ,(debug-dump (consequence tern))
      else: ,(debug-dump (alternative tern))))

;;
;; barewords
;;

  (define-class <ast-bareword> ()
    ((location)
     (value)))

  (define-method (debug-dump (bw <ast-bareword>))
    (string->symbol (slot-value bw 'value)))

;;
;; lexical variables
;;

  (define-class <ast-variable-lexical> ()
    ((location)
     (value)))

  (define-method (debug-dump (var <ast-variable-lexical>))
    `(lex ,(string->symbol (slot-value var 'value))))

;;
;; assignments
;;

  (define-class <ast-assign> ()
    ((location)
     (target     reader: target)
     (expression reader: expression)))

  (define-method (debug-dump (assign <ast-assign>))
    `(assign ,(debug-dump (target assign))
             ,(debug-dump (expression assign))))

;;
;; equalities
;;

  (define-class <ast-equality-operation> ()
    ((items reader: items)))

  (define-method (debug-dump (eq <ast-equality-operation>))
    `(equality ,@(map (lambda (item)
                        (if (string? item)
                          item
                          (debug-dump item)))
                      (items eq))))

;;
;; method refs
;;

  (define-class <ast-method-ref> ()
    ((location)
     (is-maybe?     reader: is-maybe?)
     (invocant      reader: invocant)
     (method        reader: method)
     (arguments     reader: arguments)))

  (define-method (debug-dump (mc <ast-method-ref>))
    `(call-method
      ,@(if (is-maybe? mc)   '(maybe:)   '())
      ,(debug-dump (invocant mc))
      ,(debug-dump (method mc))
      ,(debug-dump (arguments mc))))

  (define (make-method-ref op inv met maybe args)
    (make <ast-method-ref>
      'location     (token-location op)
      'is-maybe?    maybe
      'invocant     inv
      'method       met
      'arguments    args))

;;
;; method call
;;

  (define-class <ast-method-call> ()
    ((location)
     (is-maybe?     reader: is-maybe?)
     (is-chained?   reader: is-chained?)
     (invocant      reader: invocant)
     (method        reader: method)
     (arguments     reader: arguments)))

  (define-method (debug-dump (mc <ast-method-call>))
    `(call-method
      ,@(if (is-maybe? mc)   '(maybe:)   '())
      ,@(if (is-chained? mc) '(chained:) '())
      ,(debug-dump (invocant mc))
      ,(debug-dump (method mc))
      ,(debug-dump (arguments mc))))

  (define (make-method-call op inv met maybe chained args)
    (make <ast-method-call>
      'location     (token-location op)
      'is-maybe?    maybe
      'is-chained?  chained
      'invocant     inv
      'method       met
      'arguments    args))

;;
;; named values
;;

  (define-class <ast-named-value> ()
    ((location)
     (name  reader: name)
     (value reader: value)))

  (define-method (debug-dump (nv <ast-named-value>))
    `(named
      ,(debug-dump (name nv))
      ,(debug-dump (value nv))))

  (define (make-named-value op name value)
    (make <ast-named-value>
      'location (token-location op)
      'name     name
      'value    value))

;;
;; arrays
;;

  (define-class <ast-array> ()
    ((location)
     (items reader: items)))

  (define-method (debug-dump (array <ast-array>))
    `(array ,@(map debug-dump (items array))))

  (define (make-array op items)
    (make <ast-array>
      'location (token-location op)
      'items    items))

;;
;; arguments
;;

  (define-class <ast-arguments> ()
    ((items reader: items)))

  (define-method (debug-dump (args <ast-arguments>))
    `(args ,@(map (lambda (i) (debug-dump i)) (items args))))

  (define (make-arguments ls)
    (make <ast-arguments>
      'items ls))

;;
;; identifiers
;;

  (define-class <ast-identifier> ()
    ((location)
     (value reader: value)))

  (define-method (debug-dump (id <ast-identifier>))
    `(ident ,(value id)))

  (define (make-identifier token)
    (make <ast-identifier>
      'location (token-location token)
      'value    (token-value token)))

;;
;; strings
;;

  (define-class <ast-string> ()
    ((location)
     (value reader: value)))

  (define-method (debug-dump (str <ast-string>))
    `(str ,(value str)))

  (define (make-string-value token)
    (make <ast-string>
      'location (token-location token)
      'value    (token-value token)))

  (define (identifier->string ident)
    (make <ast-string>
      'location (slot-value ident 'location)
      'value    (value ident)))

;;
;; splices
;;

  (define-class <ast-splice-hash> ()
    ((location)
     (expression reader: expression)))

  (define-method (debug-dump (sp <ast-splice-hash>))
    `(% ,(debug-dump (expression sp))))

  (define (make-hash-splice token expr)
    (make <ast-splice-hash>
      'location     (token-location token)
      'expression   expr))

  (define-class <ast-splice-array> ()
    ((location)
     (expression reader: expression)))

  (define-method (debug-dump (sp <ast-splice-array>))
    `(@ ,(debug-dump (expression sp))))

  (define (make-array-splice token expr)
    (make <ast-splice-array>
      'location     (token-location token)
      'expression   expr))

;;
;; generators
;;

  (define (make-number token)
    (make <ast-number>
      'value    (token-value token)
      'location (token-location token)))

  (define (make-statement expression)
    (make <ast-statement>
      'expression expression))

  (define (make-binary-operator op left right)
    (make <ast-binary-operator>
      'location (token-location op)
      'operator (token-value op)
      'left     left
      'right    right))

  (define (make-unary-operator op operand pos)
    (make <ast-unary-operator>
      'location (token-location op)
      'operator (token-value op)
      'operand  operand
      'position pos))

  (define (make-ternary-operator op condition conseq alter)
    (make <ast-ternary-operator>
      'location     (token-location op)
      'condition    condition
      'consequence  conseq
      'alternative  alter))

  (define (make-bareword token)
    (make <ast-bareword>
      'location (token-location token)
      'value    (token-value token)))

  (define (make-lexical-variable token)
    (make <ast-variable-lexical>
      'location (token-location token)
      'value    (token-value token)))

  (define (make-assign op target expression)
    (make <ast-assign>
      'location   (token-location op)
      'target     target
      'expression expression))

  (define assign-expand
    '(("+="  "+")
      ("-="  "-")
      ("*="  "*")
      ("/="  "/")
      ("~="  "~")
      ("//=" "//")
      ("||=" "||")
      ("&&=" "&&")))

  (define (make-assign/sc op target expression)
    (make <ast-assign>
      'location   (token-location op)
      'target     target
      'expression (make <ast-binary-operator>
                    'location (token-location op)
                    'operator (cadr (assoc (token-value op)
                                          assign-expand))
                    'left     target
                    'right    expression)))

  (define (make-equality-operations op left right)
    (make <ast-equality-operation>
      'items (list left (token-value op) right)))

  (define (combine-equality-operations op left-eq right)
    (make <ast-equality-operation>
      'items (append (slot-value left-eq 'items)
                     (list (token-value op) right))))

  (define (make-document statements)
    (make <ast-document>
      'statements statements)))
