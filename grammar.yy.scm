(require-extension lalr-driver)
(define parser
  (lr-driver
    '#(((*default* -3)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* -31))
       ((*default* -28))
       ((*default* -30))
       ((*default* -29))
       ((*default* -27) (OP_ASSIGN_SC -25) (OP_ASSIGN -25))
       ((*default* -11))
       ((*default* *error*) (OP_ASSIGN 19) (OP_ASSIGN_SC 18))
       ((*default* -8)
        (SEMICOLON 29)
        (OP_L_OR 28)
        (OP_L_ERR 27)
        (OP_L_AND 26)
        (OP_TERN_THEN 25)
        (OP_H_OR 24)
        (OP_H_ERR 23)
        (OP_H_AND 22)
        (OP_EQUAL 21)
        (OP_COMPARE 20))
       ((*default* -12) (OP_EQUAL 31))
       ((*default* -2))
       ((*default* *error*) (*eoi* 32))
       ((*default* -16))
       ((*default* -17)
        (OP_COMPARE 20)
        (OP_EQUAL 21)
        (OP_H_AND 22)
        (OP_H_ERR 23)
        (OP_H_OR 24)
        (OP_TERN_THEN 25))
       ((*default* *error*)
        (PARENS_R 33)
        (OP_L_OR 28)
        (OP_L_ERR 27)
        (OP_L_AND 26)
        (OP_TERN_THEN 25)
        (OP_H_OR 24)
        (OP_H_ERR 23)
        (OP_H_AND 22)
        (OP_EQUAL 21)
        (OP_COMPARE 20))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* -7)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* -4))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* -1) (*eoi* accept))
       ((*default* -26))
       ((*default* -15)
        (OP_COMPARE 20)
        (OP_EQUAL 21)
        (OP_H_AND 22)
        (OP_H_ERR 23)
        (OP_H_OR 24)
        (OP_TERN_THEN 25))
       ((*default* -14)
        (OP_COMPARE 20)
        (OP_EQUAL 21)
        (OP_H_AND 22)
        (OP_H_ERR 23)
        (OP_H_OR 24)
        (OP_TERN_THEN 25))
       ((*default* -18))
       ((*default* -10))
       ((*default* -19) (OP_COMPARE 20) (OP_EQUAL 21))
       ((*default* -21) (OP_COMPARE 20) (OP_EQUAL 21) (OP_H_AND 22))
       ((*default* -20) (OP_COMPARE 20) (OP_EQUAL 21) (OP_H_AND 22))
       ((*default* *error*)
        (OP_L_OR 28)
        (OP_L_ERR 27)
        (OP_L_AND 26)
        (OP_TERN_THEN 25)
        (OP_TERN_ELSE 47)
        (OP_H_OR 24)
        (OP_H_ERR 23)
        (OP_H_AND 22)
        (OP_EQUAL 21)
        (OP_COMPARE 20))
       ((*default* -22)
        (OP_COMPARE 20)
        (OP_EQUAL 21)
        (OP_H_AND 22)
        (OP_H_ERR 23)
        (OP_H_OR 24)
        (OP_TERN_THEN 25))
       ((*default* -24)
        (OP_COMPARE 20)
        (OP_EQUAL 21)
        (OP_H_AND 22)
        (OP_H_ERR 23)
        (OP_H_OR 24)
        (OP_TERN_THEN 25)
        (OP_L_AND 26))
       ((*default* -23)
        (OP_COMPARE 20)
        (OP_EQUAL 21)
        (OP_H_AND 22)
        (OP_H_ERR 23)
        (OP_H_OR 24)
        (OP_TERN_THEN 25)
        (OP_L_AND 26))
       ((*default* -8)
        (SEMICOLON 29)
        (OP_L_OR 28)
        (OP_L_ERR 27)
        (OP_L_AND 26)
        (OP_TERN_THEN 25)
        (OP_H_OR 24)
        (OP_H_ERR 23)
        (OP_H_AND 22)
        (OP_EQUAL 21)
        (OP_COMPARE 20))
       ((*default* -9))
       ((*default* *error*)
        (INT 7)
        (FLOAT 6)
        (BAREWORD 5)
        (LEXVAR 4)
        (PARENS_L 3)
        (OP_L_NOT 2)
        (OP_H_NOT 1))
       ((*default* -6))
       ((*default* -13)
        (OP_COMPARE 20)
        (OP_EQUAL 21)
        (OP_H_AND 22)
        (OP_H_ERR 23)
        (OP_H_OR 24)
        (OP_TERN_THEN 25)))
    (vector
      '((8 . 8) (7 . 9) (6 . 10) (5 . 11) (4 . 12) (2 . 13) (1 . 14))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 15) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 16) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 17) (4 . 12))
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '((3 . 30))
      '()
      '()
      '()
      '()
      '()
      '()
      '((8 . 8) (7 . 9) (6 . 10) (5 . 34) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 35) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 36) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 37) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 38) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 39) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 40) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 41) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 42) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 43) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 44) (4 . 12))
      '((8 . 8) (7 . 9) (6 . 10) (5 . 45) (4 . 12))
      '()
      '((8 . 8) (7 . 9) (6 . 10) (5 . 46) (4 . 12))
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
      '((3 . 48))
      '()
      '((8 . 8) (7 . 9) (6 . 10) (5 . 49) (4 . 12))
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

