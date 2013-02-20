(require-extension lalr-driver)
(define parser
  (lr-driver
    '#(((*default* -3) (INT 5) (FLOAT 4) (BAREWORD 3) (LEXVAR 2) (OP_L_NOT 1))
       ((*default* *error*)
        (INT 5)
        (FLOAT 4)
        (BAREWORD 3)
        (LEXVAR 2)
        (OP_L_NOT 1))
       ((*default* -20))
       ((*default* -17))
       ((*default* -19))
       ((*default* -18))
       ((*default* -16) (OP_ASSIGN -15))
       ((*default* -9))
       ((*default* *error*) (OP_ASSIGN 13))
       ((*default* -8) (SEMICOLON 17) (OP_L_OR 16) (OP_L_ERR 15) (OP_L_AND 14))
       ((*default* -2))
       ((*default* *error*) (*eoi* 19))
       ((*default* -11))
       ((*default* *error*)
        (INT 5)
        (FLOAT 4)
        (BAREWORD 3)
        (LEXVAR 2)
        (OP_L_NOT 1))
       ((*default* *error*)
        (INT 5)
        (FLOAT 4)
        (BAREWORD 3)
        (LEXVAR 2)
        (OP_L_NOT 1))
       ((*default* *error*)
        (INT 5)
        (FLOAT 4)
        (BAREWORD 3)
        (LEXVAR 2)
        (OP_L_NOT 1))
       ((*default* *error*)
        (INT 5)
        (FLOAT 4)
        (BAREWORD 3)
        (LEXVAR 2)
        (OP_L_NOT 1))
       ((*default* -7) (INT 5) (FLOAT 4) (BAREWORD 3) (LEXVAR 2) (OP_L_NOT 1))
       ((*default* -4))
       ((*default* -1) (*eoi* accept))
       ((*default* -10))
       ((*default* -12))
       ((*default* -14) (OP_L_AND 14))
       ((*default* -13) (OP_L_AND 14) (OP_L_ERR 15))
       ((*default* -8) (SEMICOLON 17) (OP_L_OR 16) (OP_L_ERR 15) (OP_L_AND 14))
       ((*default* -6)))
    (vector
      '((7 . 6) (6 . 7) (5 . 8) (4 . 9) (2 . 10) (1 . 11))
      '((7 . 6) (6 . 7) (5 . 8) (4 . 12))
      '()
      '()
      '()
      '()
      '()
      '()
      '()
      '((3 . 18))
      '()
      '()
      '()
      '((7 . 6) (6 . 7) (5 . 8) (4 . 20))
      '((7 . 6) (6 . 7) (5 . 8) (4 . 21))
      '((7 . 6) (6 . 7) (5 . 8) (4 . 22))
      '((7 . 6) (6 . 7) (5 . 8) (4 . 23))
      '((7 . 6) (6 . 7) (5 . 8) (4 . 24))
      '()
      '()
      '()
      '()
      '()
      '()
      '((3 . 25))
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
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 4 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 4 (make-assign $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($2 (vector-ref ___stack (- ___sp 1)))
               ($1 (vector-ref ___stack (- ___sp 3))))
          (___push 2 4 (make-unary-operator $1 $2 'prefix))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 4 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 4 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($3 (vector-ref ___stack (- ___sp 1)))
               ($2 (vector-ref ___stack (- ___sp 3)))
               ($1 (vector-ref ___stack (- ___sp 5))))
          (___push 3 4 (make-binary-operator $2 $1 $3))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 5 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1)))) (___push 1 6 $1)))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 6 (make-bareword $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 6 (make-number $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 6 (make-number $1))))
      (lambda (___stack ___sp ___goto-table ___push yypushback)
        (let* (($1 (vector-ref ___stack (- ___sp 1))))
          (___push 1 7 (make-lexical-variable $1)))))))

