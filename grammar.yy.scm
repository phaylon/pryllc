(require-extension lalr-driver)
(define parser
  (lr-driver
    '#(((*default* -3)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* -37))
       ((*default* -34))
       ((*default* -36))
       ((*default* -35))
       ((*default* -33) (OP_ASSIGN_SC -31) (OP_ASSIGN -31))
       ((*default* -11))
       ((*default* *error*) (OP_ASSIGN 23) (OP_ASSIGN_SC 22))
       ((*default* -8)
        (SEMICOLON 37)
        (OP_L_OR 36)
        (OP_L_ERR 35)
        (OP_L_AND 34)
        (OP_TERN_THEN 33)
        (OP_H_OR 32)
        (OP_H_ERR 31)
        (OP_H_AND 30)
        (OP_EQUAL 29)
        (OP_COMPARE 28)
        (OP_CONCAT 27)
        (OP_PLUS 26)
        (OP_MINUS 25)
        (OP_H_MATH 24))
       ((*default* -12) (OP_EQUAL 39))
       ((*default* -2))
       ((*default* *error*) (*eoi* 40))
       ((*default* -18))
       ((*default* -19) (OP_H_MATH 24))
       ((*default* -16)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27))
       ((*default* -17))
       ((*default* *error*)
        (PARENS_R 41)
        (OP_L_OR 36)
        (OP_L_ERR 35)
        (OP_L_AND 34)
        (OP_TERN_THEN 33)
        (OP_H_OR 32)
        (OP_H_ERR 31)
        (OP_H_AND 30)
        (OP_EQUAL 29)
        (OP_COMPARE 28)
        (OP_CONCAT 27)
        (OP_PLUS 26)
        (OP_MINUS 25)
        (OP_H_MATH 24))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* -7)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* -4))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* -1) (*eoi* accept))
       ((*default* -32))
       ((*default* -15)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30)
        (OP_H_ERR 31)
        (OP_H_OR 32)
        (OP_TERN_THEN 33))
       ((*default* -14)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30)
        (OP_H_ERR 31)
        (OP_H_OR 32)
        (OP_TERN_THEN 33))
       ((*default* -20))
       ((*default* -21) (OP_H_MATH 24))
       ((*default* -22) (OP_H_MATH 24))
       ((*default* -23) (OP_H_MATH 24) (OP_MINUS 25) (OP_PLUS 26))
       ((*default* -24)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27))
       ((*default* -10)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27))
       ((*default* -25)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29))
       ((*default* -27)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30))
       ((*default* -26)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30))
       ((*default* *error*)
        (OP_L_OR 36)
        (OP_L_ERR 35)
        (OP_L_AND 34)
        (OP_TERN_THEN 33)
        (OP_TERN_ELSE 59)
        (OP_H_OR 32)
        (OP_H_ERR 31)
        (OP_H_AND 30)
        (OP_EQUAL 29)
        (OP_COMPARE 28)
        (OP_CONCAT 27)
        (OP_PLUS 26)
        (OP_MINUS 25)
        (OP_H_MATH 24))
       ((*default* -28)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30)
        (OP_H_ERR 31)
        (OP_H_OR 32)
        (OP_TERN_THEN 33))
       ((*default* -30)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30)
        (OP_H_ERR 31)
        (OP_H_OR 32)
        (OP_TERN_THEN 33)
        (OP_L_AND 34))
       ((*default* -29)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30)
        (OP_H_ERR 31)
        (OP_H_OR 32)
        (OP_TERN_THEN 33)
        (OP_L_AND 34))
       ((*default* -8)
        (SEMICOLON 37)
        (OP_L_OR 36)
        (OP_L_ERR 35)
        (OP_L_AND 34)
        (OP_TERN_THEN 33)
        (OP_H_OR 32)
        (OP_H_ERR 31)
        (OP_H_AND 30)
        (OP_EQUAL 29)
        (OP_COMPARE 28)
        (OP_CONCAT 27)
        (OP_PLUS 26)
        (OP_MINUS 25)
        (OP_H_MATH 24))
       ((*default* -9)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27))
       ((*default* *error*)
        (INT 9)
        (FLOAT 8)
        (BAREWORD 7)
        (LEXVAR 6)
        (PARENS_L 5)
        (OP_L_NOT 4)
        (OP_H_NOT 3)
        (OP_PLUS 2)
        (OP_MINUS 1))
       ((*default* -6))
       ((*default* -13)
        (OP_H_MATH 24)
        (OP_MINUS 25)
        (OP_PLUS 26)
        (OP_CONCAT 27)
        (OP_COMPARE 28)
        (OP_EQUAL 29)
        (OP_H_AND 30)
        (OP_H_ERR 31)
        (OP_H_OR 32)
        (OP_TERN_THEN 33)))
    (vector
      '((8 . 10) (7 . 11) (6 . 12) (5 . 13) (4 . 14) (2 . 15) (1 . 16))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 17) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 18) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 19) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 20) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 21) (4 . 14))
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '((3 . 38))
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '((8 . 10) (7 . 11) (6 . 12) (5 . 42) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 43) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 44) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 45) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 46) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 47) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 48) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 49) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 50) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 51) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 52) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 53) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 54) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 55) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 56) (4 . 14))
      '((8 . 10) (7 . 11) (6 . 12) (5 . 57) (4 . 14))
      '()
      '((8 . 10) (7 . 11) (6 . 12) (5 . 58) (4 . 14))
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '((3 . 60))
      '()
      '((8 . 10) (7 . 11) (6 . 12) (5 . 61) (4 . 14))
      '()
      '())
    (vector
      '()
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          $1))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 1 (make-document $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* () (___push 0 1 (make-document '()))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 2 (cons (make-statement $1) $2))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* () (___push 0 2 '())))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 3 (cons (make-statement $2) $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 3 '())))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* () (___push 0 3 '())))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 4 (combine-equality-operations $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 4 (make-equality-operations $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 5 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 5 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($5 (vector-ref ___stack (- ___sp 1)))
               ($4 (vector-ref ___stack (- ___sp 3)))
               ($3 (vector-ref ___stack (- ___sp 5)))
               ($2 (vector-ref ___stack (- ___sp 7)))
               ($1 (vector-ref ___stack (- ___sp 9))))
          (___push 5 5 (make-ternary-operator $2 $1 $3 $5))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-assign $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-assign/sc $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 5 (make-unary-operator $1 $2 'prefix))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 5 (make-unary-operator $1 $2 'prefix))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 5 (make-unary-operator $1 $2 'prefix))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 5 (make-unary-operator $1 $2 'prefix))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 5 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 6 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 7 $2)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 7 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 7 (make-bareword $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 7 (make-number $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 7 (make-number $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 8 (make-lexical-variable $1)))))))

