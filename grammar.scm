(require-extension lalr)

(lalr-parser
  (output: parser "grammar.yy.scm")
  (expect: 5)

  (INT FLOAT BAREWORD LEXVAR
   SEMICOLON
   PARENS_L PARENS_R
   (left:  OP_L_OR OP_L_ERR)
   (left:  OP_L_AND)
   (right: OP_L_NOT)
   (right: OP_ASSIGN OP_ASSIGN_SC)
   (right: OP_TERN_THEN OP_TERN_ELSE)
   (left:  OP_H_OR OP_H_ERR)
   (left:  OP_H_AND)
   (left:  OP_EQUAL OP_COMPARE)
   (right: OP_H_NOT)
   (left:  OP_CONCAT)
   (left:  OP_L_MATH)
   (left:  OP_H_MATH))

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

  (expression
        (atom)
            : $1
        (op-equal-binary)
            : $1
        (expression OP_TERN_THEN expression OP_TERN_ELSE expression)
            : (make-ternary-operator $2 $1 $3 $5)
        (assignable OP_ASSIGN expression)
            : (make-assign $2 $1 $3)
        (assignable OP_ASSIGN_SC expression)
            : (make-assign/sc $2 $1 $3)
        (OP_H_NOT expression)
            : (make-unary-operator $1 $2 'prefix)
        (OP_L_NOT expression)
            : (make-unary-operator $1 $2 'prefix)
        (expression OP_H_MATH expression)
            : (make-binary-operator $2 $1 $3)
        (expression OP_L_MATH expression)
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
        (lexical-variable)
            : $1)

  (atom
        (PARENS_L expression PARENS_R)
            : $2
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
