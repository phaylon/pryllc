(require-extension coops srfi-1 srfi-13)

(load "ast")
(load "parser.scm")
(import parsing)
(import ast)
(import chicken scheme)

(define t/level 0)
(define t/fail-cnt 0)
(define t/pass-cnt 0)

(define (t/eq a b) (equal? (->string a) (->string b)))

(define (text . args) (string-concatenate (map ->string args)))

(define (t/class class)
  (string-concatenate
    (list
      "is a "
      (->string (class-name class)))))

(define (t/done)
  (t/diag "---")
  (t/diag "pass: " t/pass-cnt)
  (if (> t/fail-cnt 0)
    (t/diag "fail: " t/fail-cnt)
    #f))

(define (t/indent)
  (string-concatenate (append '("") (make-list t/level "  "))))

(define t/next-index
  (let ((i 0))
    (lambda ()
      (let ((n (+ i 1)))
        (set! i n)
        i))))

(define (t/pass title)
  (set! t/pass-cnt (+ t/pass-cnt 1))
  (map display (list (t/indent) "ok " (t/next-index) " - " title))
  (newline)
  #t)

(define (t/fail title)
  (set! t/fail-cnt (+ t/fail-cnt 1))
  (map display (list (t/indent) "not ok " (t/next-index) " - " title))
  (newline)
  (error "test failed")
  #f)

(define (t/ok value title)
  (if value
    (t/pass title)
    (t/fail title)))

(define (t/diag . args)
  (map display `(,(t/indent) "# " ,@args))
  (newline)
  #f)

(define (t/true-list ls)
  (if (> (length ls) 0)
    (= 0 (length (filter (lambda (n) (not n)) ls)))
    #t))

(define (t/group title . tests)
  (t/diag title)
  (set! t/level (+ t/level 1))
  (let ((r (t/true-list (map (lambda (f) (f)) tests))))
    (set! t/level (- t/level 1))
    (t/ok r title)))

(define (cb/group title . tests)
  (lambda (value)
    (t/group
      title
      (lambda ()
        (map (lambda (t) (t value)) tests)))))

(define (is-object obj class . then)
  (or (and (t/ok (subclass? (class-of obj) class) (t/class class))
           (t/true-list (map (lambda (t) (t obj)) then)))
      (t/diag "received: " obj)))

(define (is-equal op value expected title)
  (if (op value expected)
    (t/pass title)
    (begin
      (t/diag "expected: " expected)
      (t/diag "received: " value)
      (t/fail title)
      #f)))

(define (test-all vs ts)
  (and (is-equal equal? (length vs) (length ts) "item count")
       (t/true-list 
         (map (lambda (pair) ((cadr pair) (car pair)))
              (zip vs ts)))))

(define (cb/is op expected)
  (lambda (value)
    (is-equal op value expected "value")))

(define (cb/list title . item-tests)
  (lambda (value)
    (t/group title
      (lambda ()
        (if (list? value)
          (begin
            (t/pass "is a list")
            (test-all value item-tests))
          (t/fail "is a list"))))))
          
(define (cb/object class . then)
  (lambda (value)
    (apply is-object value class then)))

(define (cb/slot slot test)
  (lambda (obj)
    (t/group
      slot
      (lambda ()
        (test (slot-value obj slot))))))

(define (cb/ast title source . statement-tests)
  (lambda ()
    (let ((ast (source->ast title source)))
      (t/group
        title
        (lambda ()
          (is-object
            ast
            <ast-document>
            (cb/slot
              'statements
              (apply
                cb/list
                "statement tests"
                (map (lambda (test)
                       (cb/object
                         <ast-statement>
                         (cb/slot
                           'expression
                           test)))
                     statement-tests)))))))))

(define (ast/number num)
  (cb/object <ast-number> (cb/slot 'value (cb/is t/eq num))))

(define (ast/binop op left right)
  (cb/object
    <ast-binary-operator>
    (cb/slot 'operator (cb/is t/eq op))
    (cb/slot 'left     left)
    (cb/slot 'right    right)))

(define (ast/unop op operand)
  (cb/object
    <ast-unary-operator>
    (cb/slot 'operator (cb/is t/eq op))
    (cb/slot 'operand  operand)))

(define (ast/assign target source)
  (cb/object
    <ast-assign>
    (cb/slot 'target     target)
    (cb/slot 'expression source)))

(define (ast/lexvar name)
  (cb/object
    <ast-variable-lexical>
    (cb/slot 'value (cb/is t/eq name))))

(define (ast/ternop condit conseq alter)
  (cb/object
    <ast-ternary-operator>
    (cb/slot 'condition condit)
    (cb/slot 'consequence conseq)
    (cb/slot 'alternative alter)))

(define (ast/eq . items)
  (cb/object
    <ast-equality-operation>
    (cb/slot
      'items
      (apply
        cb/list
        "operations"
        (map (lambda (item)
               (if (string? item)
                 (cb/is t/eq item)
                 item))
             items)))))

(define ast/$a (ast/lexvar "$a"))
(define ast/$b (ast/lexvar "$b"))
(define ast/$c (ast/lexvar "$c"))
(define ast/$d (ast/lexvar "$d"))
(define ast/$e (ast/lexvar "$e"))

(define (g/ast/variables)
  (t/group "variables"
    (cb/ast "simple lexical" "$foo" (ast/lexvar "$foo"))
    (cb/ast "underscore lexical" "$_foo" (ast/lexvar "$_foo"))
    (cb/ast "topic lexical" "$_" (ast/lexvar "$_"))
    (cb/ast "numbered lexival" "$v23" (ast/lexvar "$v23"))))

(define (g/ast/numbers)
  (t/group "numbers"
    (cb/ast "simple integer" "23" (ast/number 23))
    (cb/ast "simple float" "23.5" (ast/number 23.5))))

(define (g/ast/document)
  (t/group "document"
    (cb/ast
      "multiple statements"
      "23; 42; 17"
      (ast/number 23)
      (ast/number 42)
      (ast/number 17))
    (cb/ast
      "empty document"
      "")))

(define (g/ast/groupings)
  (t/group "groupings"
    (cb/ast
      "left grouped"
      "(23 and 17) and 42"
      (ast/binop
        "and"
        (ast/binop "and" (ast/number 23) (ast/number 17))
        (ast/number 42)))
    (cb/ast
      "right grouped"
      "23 and (17 and 42)"
      (ast/binop
        "and"
        (ast/number 23)
        (ast/binop "and" (ast/number 17) (ast/number 42))))
    (cb/ast
      "multiple groupings"
      "(23 and 17) and (5 and 42)"
      (ast/binop
        "and"
        (ast/binop "and" (ast/number 23) (ast/number 17))
        (ast/binop "and" (ast/number 5) (ast/number 42))))
    (cb/ast
      "nested groupings"
      "((23 and (17 and 5)) and 42)"
      (ast/binop
        "and"
        (ast/binop
          "and"
          (ast/number 23)
          (ast/binop "and" (ast/number 17) (ast/number 5)))
        (ast/number 42)))))

(define (g/ast/operators/ternary)
  (t/group "ternary"
    (cb/ast
      "simple"
      "$a ?? $b !! $c"
      (ast/ternop ast/$a ast/$b ast/$c))
    (cb/ast
      "chained"
      "$a ?? $b !! $c ?? $d !! $e"
      (ast/ternop ast/$a ast/$b (ast/ternop ast/$c ast/$d ast/$e)))
    (cb/ast
      "nested"
      "$a ?? $b ?? $c !! $d !! $e"
      (ast/ternop ast/$a (ast/ternop ast/$b ast/$c ast/$d) ast/$e))))

(define (g/ast/operators/equality)
  (apply t/group
    "equality"
    (map (lambda (ops)
           (let ((a (car ops))
                 (b (cadr ops))
                 (c (caddr ops)))
             (cb/ast
               (text "chained " a " and " b " and " c)
               (text "$a " a " $b " b " $c " c " $d")
               (ast/eq ast/$a a ast/$b b ast/$c c ast/$d))))
         '((">" "<" ">=")
           ("<=" "==" "!=")
           ("gt" "lt" "ge")
           ("le" "eq" "ne")
           (">" "eq" ">=")))))
         

(define (op-group/binary/left title op)
  (t/group
    title
    (cb/ast
      (text "single " title " operator")
      (text "23 " op " 17")
      (ast/binop op (ast/number 23) (ast/number 17)))
    (cb/ast
      (text "multiple " title " operators")
      (text "23 " op " 17 " op " 42")
      (ast/binop
        op
        (ast/binop
          op
          (ast/number 23)
          (ast/number 17))
        (ast/number 42)))))

(define (op-group/unary/prefix title op)
  (t/group
    title
    (cb/ast
      (text "single " title " operator")
      (text op " 23")
      (ast/unop op (ast/number 23)))
    (cb/ast
      (text "multiple " title " operators")
      (text op " 23 and " op " 17")
      (ast/binop
        "and"
        (ast/unop op (ast/number 23))
        (ast/unop op (ast/number 17))))))

(define (g/ast/operators/assign/sc)
  (apply t/group
         "shortcut assignments"
         (apply
           append
           (map (lambda (pair)
                  (let ((sc (car pair))
                        (op (cadr pair)))
                    (list
                      (cb/ast
                        (text "single '" sc "' assignment")
                        (text "$foo " sc " 23")
                        (ast/assign
                          (ast/lexvar "$foo")
                          (ast/binop
                            op
                            (ast/lexvar "$foo")
                            (ast/number 23))))
                      (cb/ast
                        (text "multiple '" sc "' assignments")
                        (text "$foo " sc " $bar " sc " 23")
                        (ast/assign
                          (ast/lexvar "$foo")
                          (ast/binop
                            op
                            (ast/lexvar "$foo")
                            (ast/assign
                              (ast/lexvar "$bar")
                              (ast/binop
                                op
                                (ast/lexvar "$bar")
                                (ast/number 23)))))))))
                '(("+="  "+")
                  ("-="  "-")
                  ("*="  "*")
                  ("/="  "/")
                  ("~~=" "~~")
                  ("||=" "||")
                  ("//=" "//")
                  ("&&=" "&&"))))))
                  

(define (g/ast/operators/low-or)
  (op-group/binary/left "low or" "or"))

(define (g/ast/operators/low-err)
  (op-group/binary/left "low err" "err"))

(define (g/ast/operators/low-and)
  (op-group/binary/left "low and" "and"))

(define (g/ast/operators/high-or)
  (op-group/binary/left "high or" "||"))

(define (g/ast/operators/high-err)
  (op-group/binary/left "high err" "//"))

(define (g/ast/operators/high-and)
  (op-group/binary/left "high and" "&&"))

(define (g/ast/operators/low-not)
  (op-group/unary/prefix "low not" "not"))

(define (g/ast/operators/assign)
  (t/group
    "assignment"
    (cb/ast
      "single assignment"
      "$foo = 23"
      (ast/assign (ast/lexvar "$foo") (ast/number 23)))
    (cb/ast
      "multiple assignments"
      "$foo = $bar = 23"
      (ast/assign
        (ast/lexvar "$foo")
        (ast/assign
          (ast/lexvar "$bar")
          (ast/number 23))))))

(define (g/ast/operators)
  (t/group
    "operators"
    g/ast/operators/low-or
    g/ast/operators/low-err
    g/ast/operators/low-and
    g/ast/operators/low-not
    g/ast/operators/assign
    g/ast/operators/assign/sc
    g/ast/operators/ternary
    g/ast/operators/high-or
    g/ast/operators/high-err
    g/ast/operators/high-and
    g/ast/operators/equality))

(define (g/ast)
  (t/group
    "ast"
    g/ast/numbers
    g/ast/document
    g/ast/operators
    g/ast/variables
    g/ast/groupings))

(g/ast)

(t/done)
