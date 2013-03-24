(declare (unit parser))
(declare (uses ast))

(import scheme)
(import srfi-1 srfi-13)
(import chicken)
(require-extension srfi-1 srfi-13 srfi-69 lalr-driver irregex)

(include "lib/grammar.scm.yy")

(declare (export source->ast))

(define digits '(+ (/ "09")))
(define bareword
  '(: (+ (or (/ "az") (/ "AZ") "_"))
      (* (or (/ "az") (/ "AZ") (/ "09") "_" "-"))))
(define opword
  '(+ ("+-*/=<>~|&?!@%")))
(define lexvar
  `(: "$" ,bareword))
(define string-single
  '(: #\' (*? any) (neg-look-behind #\\) #\'))
(define string-double
  '(: #\" (*? any) (neg-look-behind #\\) #\"))

(define token-patterns
  (map (lambda (token)
         (list (car token)
               (sre->irregex `(: bos ,(cadr token)))))
       `((DISCARD          whitespace)
         (DISCARD          (: "#" (*? any) eol))
         (LEXVAR           ($ ,lexvar))
         (BAREWORD         ($ ,bareword))
         (STRING_SINGLE    ($ ,string-single))
         (STRING_DOUBLE    ($ ,string-double))
         (OP_ASSIGN_SC     ($ (or "//=" "||=" "&&=")))
         (OP_COMPARE       ($ "<=>"))
         (OP_METHOD_REF    ($ ".&"))
         (OP_NETHER_CALL   ($ ".^"))
         (OP_TERN_THEN     ($ "??"))
         (OP_TERN_ELSE     ($ "!!"))
         (OP_H_OR          ($ "||"))
         (OP_H_ERR         ($ "//"))
         (OP_H_AND         ($ "&&"))
         (OP_COMPARE       ($ "~~"))
         (OP_NAMESPACE     ($ "::"))
         (OP_ASSIGN_SC     ($ (or "+=" "-=" "*=" "/=" "~=")))
         (OP_EQUAL_NUM     ($ (or ">=" "<=" "==" "!=")))
         (OP_EQUAL_NUM     ($ (or ">" "<")))
         (OP_PLUS          ($ "+"))
         (OP_MINUS         ($ "-"))
         (OP_H_MATH        ($ (or "*" "/")))
         (OP_CONCAT        ($ "~"))
         (SPLICE_ARRAY     ($ "@"))
         (SPLICE_HASH      ($ "%"))
         (OP_ASSIGN        ($ "="))
         (OP_METHOD_CALL   ($ "."))
         (QMARK            ($ "?"))
         (EMARK            ($ "!"))
;         (OPWORD           ($ ,opword))
         (FLOAT            ($ ,digits "." ,digits))
         (INT              ($ ,digits))
         (COLON            ($ ":"))
         (COMMA            ($ (+ ",")))
         (PARENS_L         ($ "("))
         (PARENS_R         ($ ")"))
         (BRACKET_L        ($ "["))
         (BRACKET_R        ($ "]"))
         (BRACE_L          ($ "{"))
         (BRACE_R          ($ "}"))
         (SEMICOLON        ($ (+ ";"))))))

(define op/assign
  (list "+=" "-=" "*=" "/=" "~=" "//=" "||=" "&&="))

(define op/equality/number
  (list ">" "<" ">=" "<=" "==" "!="))

(define op/equality/string
  (list "gt" "lt" "ge" "le" "eq" "ne"))

(define op/equality/any
  (list "equals" "before" "after" "at-most" "at-least"))

(define declare/modifiers
  (list "before" "around" "after"))

(define syn/cond/test
  (list "if" "unless"))

(define-inline (clear-token-type type value)
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
           ((value-is-any op/equality/any)    'OP_EQUAL_ANY)
           ((value-is "cmp")                  'OP_COMPARE_STR)
           ((value-is "compare-to")           'OP_COMPARE_ANY)
           ((value-is "does")                 'OP_DOES)
           ((value-is "lambda")               'SYN_LAMBDA)
           ((value-is "module")               'DECLARE_MODULE)
           ((value-is "function")             'DECLARE_FUNCTION)
           ((value-is "sub")                  'DECLARE_SUB)
           ((value-is "class")                'DECLARE_CLASS)
           ((value-is "method")               'DECLARE_METHOD)
           ((value-is-any declare/modifiers)  'DECLARE_MODIFIER)
           ((value-is "with")                 'DECLARE_ROLE_APPLY)
           ((value-is "role")                 'DECLARE_ROLE)
           ((value-is "requires")             'DECLARE_REQUIRES)
           ((value-is "has")                  'DECLARE_ATTRIBUTE)
           ((value-is "my")                   'LEX_MY)
           ((value-is "let")                  'LEX_LET)
           ((value-is-any syn/cond/test)      'COND_TEST)
           ((value-is "else")                 'COND_ELSE)
           ((value-is-any op/equality/string) 'OP_EQUAL_STR)
           (else type)))
    (else type)))

(define (say . ls)
  (map display ls)
  (newline))

(define-inline (unempty lines)
  (if (null? lines)
    '("")
    lines))

(define (pryll-location->source-location loc)
  (make-source-location (car loc) (cadr loc) (caddr loc) -1 -1))

(define (source-location->pryll-location loc)
  (list (source-location-input loc)
        (source-location-line loc)
        (source-location-column loc)))

(define (source->token-iterator name body)
  (define get-location
    (let ((original body))
      (lambda ()
        (let* ((removed (irregex-replace `(: ,body eos) original ""))
               (lines   (unempty (irregex-split 'newline removed)))
               (last    (car (reverse lines)))
               (lnum    (length lines))
               (cnum    (+ 1 (string-length last))))
          `(,name ,lnum ,cnum)))))
  (define (next-token patterns)
    (if (zero? (string-length body))
      '*eoi*
      (if (null? patterns)
        (pryll:err <pryll:error-syntax>
                   location: (get-location)
                   message: (conc
                              "Unable to parse: '"
                              (car (irregex-split 'newline body))
                              "'"))
        (let* ((token   (car patterns))
               (rest    (cdr patterns))
               (type    (car token))
               (pattern (cadr token)))
          (let ((match (irregex-search pattern body)))
            (if match
              (let ((str (irregex-match-substring match))
                    (loc (get-location)))
                (set! body (irregex-replace pattern body ""))
                (if (eqv? type 'DISCARD)
                  (next-token token-patterns)
                  (begin
                    (make-lexical-token
                      (clear-token-type type str)
                      (pryll-location->source-location loc)
                      `(,str ,loc)))))
              (next-token rest)))))))
  (lambda ()
    (if (zero? (string-length body))
      '*eoi*
      (let ((found (next-token token-patterns)))
        found))))

(define (source->ast name body)
  (parser
    (source->token-iterator name body)
    (lambda (msg token)
      (pryll:err <pryll:error-syntax>
                 location: (source-location->pryll-location
                             (lexical-token-source token))
                 message: (conc
                            "Unexpected token "
                            (lexical-token-category token)
                            " '"
                            (car (lexical-token-value token))
                            "'")))))
