(require-extension lalr)

(lalr-parser
  (output: parser "grammar.yy.scm")
  (expect: 5)

  (INT FLOAT BAREWORD LEXVAR
   SEMICOLON COLON COMMA
   PARENS_L  PARENS_R
   BRACKET_L BRACKET_R
   BRACE_L   BRACE_R
   QMARK
   SPLICE_ARRAY SPLICE_HASH
   (left:  OP_L_OR OP_L_ERR)
   (left:  OP_L_AND)
   (right: OP_L_NOT)
   (right: OP_ASSIGN OP_ASSIGN_SC)
   (right: OP_TERN_THEN OP_TERN_ELSE)
   (left:  OP_H_OR OP_H_ERR)
   (left:  OP_H_AND)
   (left:  OP_EQUAL OP_COMPARE)
   (right: EMARK)
   (left:  OP_CONCAT)
   (left:  OP_PLUS OP_MINUS)
   (left:  OP_H_MATH)
   (left:  OP_METHOD_CALL OP_METHOD_REF)
   (left:  prec-slot)
   (right: prec-num-sign)
   )

  (document
        (statements)
            : (make-document $1)
        ()
            : (make-document '()))

  (statements
        (expression statements-rest)
            : (cons (make-statement $1) $2)
        ()
            : '())

  (statements-rest
        (SEMICOLON expression statements-rest)
            : (cons (make-statement $2) $3)
        (SEMICOLON)
            : '()
        ()
            : '())

  (op-equal-binary
        (op-equal-binary OP_EQUAL expression)
            : (combine-equality-operations $2 $1 $3)
        (expression OP_EQUAL expression)
            : (make-equality-operations $2 $1 $3))

  ; TODO split out identifier compare and equal ops
  (identifier-lax
        (BAREWORD)  : (make-identifier $1)
        (OP_L_OR)   : (make-identifier $1)
        (OP_L_ERR)  : (make-identifier $1)
        (OP_L_AND)  : (make-identifier $1)
        (OP_L_NOT)  : (make-identifier $1))

  (named-value
        (identifier-lax COLON expression)
            : (make-named-value $2 (identifier->string $1) $3)
        (expression COLON expression)
            : (make-named-value $2 $1 $3))

  (splice-array
        (SPLICE_ARRAY expression)
            : (make-array-splice $1 $2))

  (splice-hash
        (SPLICE_HASH expression)
            : (make-hash-splice $1 $2))

  (argument-item
        (named-value)   : $1
        (splice-array)  : $1
        (splice-hash)   : $1
        (expression)    : $1)

  (arguments-rest
        (argument-item COMMA arguments-rest)
            : (cons $1 $3)
        (argument-item)
            : (cons $1 '())
        (COMMA)
            : '()
        ()
            : '())

  (arguments
        (PARENS_L arguments-rest PARENS_R)
            : (make-arguments $2)
        ()
            : (make-arguments '()))

  (arguments-req
        (PARENS_L arguments-rest PARENS_R)
            : (make-arguments $2))

  (hash-item
        (named-value)   : $1
        (splice-hash)   : $1)

  (hash-rest
        (hash-item COMMA hash-rest)
            : (cons $1 $3)
        (hash-item)
            : (cons $1 '())
        (COMMA)
            : '()
        ()
            : '())

  (hash
        (BRACE_L hash-rest BRACE_R)
            : (make-hash $1 $2))

  (array-item
        (splice-array)  : $1
        (expression)    : $1)

  (array-rest
        (array-item COMMA array-rest)
            : (cons $1 $3)
        (array-item)
            : (cons $1 '())
        (COMMA)
            : '()
        ()
            : '())

  (array
        (BRACKET_L array-rest BRACKET_R)
            : (make-array $1 $2))

  (method
        (identifier-lax)    : (identifier->string $1)
        (lexical-variable)  : $1)

  (opt-qmark
        (QMARK)     : #t
        ()          : #f)

  (opt-emark
        (EMARK)     : #t
        ()          : #f)

  (slot-ref
        (expression BRACKET_L expression BRACKET_R)
            : (make-slot-ref $2 $1 $3))

  (expression
        (atom)
            : $1
        (op-equal-binary)
            : $1
        (expression BRACKET_L expression BRACKET_R)
            : (make-slot-ref $2 $1 $3)
        (expression
         OP_METHOD_REF
         method
         opt-qmark
         arguments)
            : (make-method-ref $2 $1 $3 $4 $5)
        (expression 
         OP_METHOD_CALL
         method
         opt-qmark
         opt-emark
         arguments)
            : (make-method-call $2 $1 $3 $4 $5 $6)
        (BAREWORD PARENS_L arguments-rest PARENS_R)
            : (make-function-call $1 (make-arguments $3))
        (expression PARENS_L arguments-rest PARENS_R)
            : (make-call $2 $1 (make-arguments $3))
        (expression OP_TERN_THEN expression OP_TERN_ELSE expression)
            : (make-ternary-operator $2 $1 $3 $5)
        (assignable OP_ASSIGN expression)
            : (make-assign $2 $1 $3)
        (assignable OP_ASSIGN_SC expression)
            : (make-assign/sc $2 $1 $3)
        (EMARK expression)
            : (make-unary-operator $1 $2 'prefix)
        (OP_L_NOT expression)
            : (make-unary-operator $1 $2 'prefix)
        (OP_MINUS expression (prec: prec-num-sign))
            : (make-unary-operator $1 $2 'prefix)
        (OP_PLUS expression (prec: prec-num-sign))
            : (make-unary-operator $1 $2 'prefix)
        (expression OP_H_MATH expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_MINUS expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_PLUS expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_CONCAT expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_COMPARE expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_H_AND expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_H_OR expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_H_ERR expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_L_AND expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_L_OR expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_L_ERR expression)
            : (make-binary-operator $2 $1 $3))

  (assignable
        (slot-ref)
            : $1
        (lexical-variable)
            : $1)

  (atom
        (PARENS_L expression PARENS_R)
            : $2
        (array)
            : $1
        (hash)
            : $1
        (lexical-variable)
            : $1
        (BAREWORD)
            : (make-bareword $1)
        (INT)
            : (make-number $1)
        (FLOAT)
            : (make-number $1))
  
  (lexical-variable
        (LEXVAR)
            : (make-lexical-variable $1)))
