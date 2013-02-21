(load "ast")

(module parsing (source->ast)
  (import scheme)
  (import srfi-1 srfi-13)
  (import chicken)
  (require-extension srfi-1 srfi-13 srfi-69 lalr-driver irregex)
 
  (import ast)

  (include "grammar.yy.scm")

  (define digits '(+ (/ "09")))
  (define bareword
    '(: (+ (or (/ "az") "_"))
        (* (or (/ "az") (/ "09") "_"))))
  (define opword
    '(+ ("+-*/=<>~|&?!.")))
  (define lexvar
    `(: "$" ,bareword))

  (define (dbg title value)
    (map display (list title " " value))
    (newline)
    value)

  (define token-patterns
    `((DISCARD      whitespace)
      (LEXVAR       ($ ,lexvar))
      (BAREWORD     ($ ,bareword))
      (OPWORD       ($ ,opword))
      (FLOAT        ($ ,digits "." ,digits))
      (INT          ($ ,digits))
      (PARENS_L     ($ "("))
      (PARENS_R     ($ ")"))
      (SEMICOLON    ($ (+ ";")))))

  (define op/assign
    (list "+=" "-=" "*=" "/=" "~=" "//=" "||=" "&&="))

  (define op/equality/number
    (list ">" "<" ">=" "<=" "==" "!="))

  (define op/equality/string
    (list "gt" "lt" "ge" "le" "eq" "ne"))

  (define (clear-token-type type value)
    (define (value-is test)
      (string=? value test))
    (define (value-is-any ls)
      (< 0 (length (filter (lambda (str) (value-is str)) ls))))
    (case type
      ((BAREWORD)
       (cond ((value-is "and")                  'OP_L_AND)
             ((value-is "or")                   'OP_L_OR)
             ((value-is "not")                  'OP_L_NOT)
             ((value-is "err")                  'OP_L_ERR)
             ((value-is "cmp")                  'OP_COMPARE)
             ((value-is-any op/equality/string) 'OP_EQUAL)
             (else type)))
      ((OPWORD)
       (cond ((value-is "=")                    'OP_ASSIGN)
             ((value-is "!")                    'OP_H_NOT)
             ((value-is "~")                    'OP_CONCAT)
             ((value-is "??")                   'OP_TERN_THEN)
             ((value-is "!!")                   'OP_TERN_ELSE)
             ((value-is "||")                   'OP_H_OR)
             ((value-is "//")                   'OP_H_ERR)
             ((value-is "&&")                   'OP_H_AND)
             ((value-is-any '("<=>" "~~"))      'OP_COMPARE)
             ((value-is "+")                    'OP_PLUS)
             ((value-is "-")                    'OP_MINUS)
             ((value-is-any '("*" "/"))         'OP_H_MATH)
             ((value-is-any op/assign)          'OP_ASSIGN_SC)
             ((value-is-any op/equality/number) 'OP_EQUAL)
             (else (error (string-concatenate
                            (list "Invalid op " value))))))
      (else type)))

  (define (say . ls)
    (map display ls)
    (newline))

  (define (unempty lines)
    (if (null? lines)
      '("")
      lines))

  (define (pryll-location->source-location loc)
    (make-source-location (car loc) (cadr loc) (caddr loc) -1 -1))

  (define (source->token-iterator name body)
    (define get-location
      (let ((original body))
        (lambda ()
          (let* ((removed (irregex-replace `(: ,body eos) ""))
                 (lines   (unempty (irregex-split 'newline removed)))
                 (last    (car (reverse lines)))
                 (lnum    (length lines))
                 (cnum    (+ 1 (string-length last))))
            `(,name ,lnum ,cnum)))))
    (define (next-token patterns)
      (if (null? patterns)
        (error "Unable to parse")
;        (error (string-concatenate (list "Unable to parse >>>" body)))
        (let* ((token   (car patterns))
               (rest    (cdr patterns))
               (type    (car token))
               (pattern `(: bos ,(cadr token))))
;          (say "test: " type)
;          (say "pattern: " pattern)
          (let ((match (irregex-search pattern body)))
            (if match
              (let ((str (irregex-match-substring match)))
                (set! body (irregex-replace pattern body ""))
                (if (eqv? type 'DISCARD)
                  (next-token token-patterns)
                  (let ((location (get-location)))
                    (make-lexical-token
                      (dbg "TYPE" (clear-token-type type str))
;                      (clear-token-type type str)
                      (pryll-location->source-location location)
                      `(,str ,location)))))
              (next-token rest))))))
    (lambda ()
;      (say "--- get token")
      (if (zero? (string-length body))
        '*eoi*
        (let ((found (next-token token-patterns)))
          found))))

  (define (source->ast name body)
    (parser
      (source->token-iterator name body)
      (lambda args
        (say args)
        (error "An error occured")))))
