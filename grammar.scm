(require-extension lalr)

(lalr-parser
  (output: parser "grammar.yy.scm")
  (expect: 5)

  (INT FLOAT BAREWORD LEXVAR
   SEMICOLON
   PARENS_L PARENS_R
   (left:  OP_L_OR)
   (left:  OP_L_ERR)
   (left:  OP_L_AND)
   (right: OP_L_NOT)
   (right: OP_ASSIGN OP_ASSIGN_SC))

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

  (expression
        (atom)
            : $1
        (assignable OP_ASSIGN expression)
            : (make-assign $2 $1 $3)
        (assignable OP_ASSIGN_SC expression)
            : (make-assign/sc $2 $1 $3)
        (OP_L_NOT expression)
            : (make-unary-operator $1 $2 'prefix)
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
