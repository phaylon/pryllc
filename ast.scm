
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

  (define (make-document statements)
    (make <ast-document>
      'statements statements)))
