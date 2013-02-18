(require-extension lalr)

(lalr-parser
  (output: parser "grammar.yy.scm")
  (expect: 5)

  (INT SEMICOLON)

  (document
    (statements)
      : (make-document $1)
    ()
      : (make-document '()))

  (statements
    (expression statement-rest)
      : (cons (make-statement $1) $2)
    ()
      : '())

  (statement-rest
    (SEMICOLON expression statement-rest)
      : (cons (make-statement $2) $3)
    (SEMICOLON)
      : '()
    ()
      : '())

  (expression
    (INT)
      : (make-integer $1)))
