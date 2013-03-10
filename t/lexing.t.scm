(require-extension srfi-1 srfi-13)

(load "dev/libs.scm")
(load "t/lib/test.scm")

(define (cb/ast title source . statement-tests)
  (lambda ()
    (let ((ast (source->ast title source)))
      (t/group
        title
        (lambda ()
          (is-object
            ast
            <pryll:ast-document>
            (cb/slot
              'statements
              (apply
                cb/list
                "statement tests"
                (map (lambda (test)
                       (cb/object
                         <pryll:ast-statement>
                         (cb/slot
                           'expression
                           test)))
                     statement-tests)))))))))

(define (ast/number num)
  (cb/object <pryll:ast-number> (cb/slot 'value (cb/is t/eq num))))

(define (ast/binop op left right)
  (cb/object
    <pryll:ast-binary-operator>
    (cb/slot 'operator (cb/is t/eq op))
    (cb/slot 'left     left)
    (cb/slot 'right    right)))

(define (ast/unop op operand)
  (cb/object
    <pryll:ast-unary-operator>
    (cb/slot 'operator (cb/is t/eq op))
    (cb/slot 'operand  operand)))

(define (ast/assign target source)
  (cb/object
    <pryll:ast-assign>
    (cb/slot 'target     target)
    (cb/slot 'expression source)))

(define (ast/lexvar name)
  (cb/object
    <pryll:ast-variable-lexical>
    (cb/slot 'value (cb/is t/eq name))))

(define (ast/ternop condit conseq alter)
  (cb/object
    <pryll:ast-ternary-operator>
    (cb/slot 'condition condit)
    (cb/slot 'consequence conseq)
    (cb/slot 'alternative alter)))

(define (ast/string value)
  (cb/object
    <pryll:ast-string>
    (cb/slot 'value (cb/is t/eq value))))

(define (ast/ident value)
  (cb/object
    <pryll:ast-identifier>
    (cb/slot 'value (cb/is t/eq value))))

(define (ast/named name value)
  (cb/object
    <pryll:ast-named-value>
    (cb/slot 'name  name)
    (cb/slot 'value value)))

(define (ast/slot-ref cont slot)
  (cb/object
    <pryll:ast-slot-ref>
    (cb/slot 'container cont)
    (cb/slot 'slot      slot)))

(define (ast/splice class expr)
  (cb/object
    class
    (cb/slot 'expression expr)))

(define (ast/splice-% expr)
  (ast/splice <pryll:ast-splice-hash> expr))
(define (ast/splice-@ expr)
  (ast/splice <pryll:ast-splice-array> expr))

(define (ast/hash . items)
  (cb/object
    <pryll:ast-hash>
    (cb/slot
      'items
      (apply
        cb/list
        "hash elements"
        items))))

(define (ast/func-call name . args)
  (cb/object
    <pryll:ast-function-call>
    (cb/slot 'function-name (cb/is t/eq name))
    (cb/slot 'arguments (apply ast/args args))))

(define (ast/call function . args)
  (cb/object
    <pryll:ast-call>
    (cb/slot 'function function)
    (cb/slot 'arguments (apply ast/args args))))

(define (ast/array . items)
  (cb/object
    <pryll:ast-array>
    (cb/slot
      'items
      (apply
        cb/list
        "array elements"
        items))))

(define (ast/args . items)
  (cb/object
    <pryll:ast-arguments>
    (cb/slot
      'items
      (apply
        cb/list
        "arguments"
        items))))

(define (ast/method-op class flags inv met . args)
  (apply
    cb/object
    class
    (append
      (map (lambda (pair)
             (let ((slot     (car pair))
                   (expected (cadr pair)))
               (cb/slot slot (cb/is equal? expected))))
           flags)
      (list
        (cb/slot 'invocant inv)
        (cb/slot 'method met)
        (cb/slot 'arguments (apply ast/args args))))))

(define (ast/method-ref maybe inv met . args)
  (apply ast/method-op
    <pryll:ast-method-ref>
    `((is-maybe ,maybe))
    inv
    met
    args))

(define (ast/method-call maybe chained inv met . args)
  (apply ast/method-op
    <pryll:ast-method-call>
    `((is-maybe ,maybe)
      (is-chained ,chained))
    inv
    met
    args))

(define (ast/eq . items)
  (cb/object
    <pryll:ast-equality-operation>
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

(define (g/ast/operators/num-signs)
  (t/group "unary number signs"
    (cb/ast
      "simple"
      "-23"
      (ast/unop "-" (ast/number 23)))
    (cb/ast
      "multiple"
      "-23 * +17 - -4"
      (ast/binop
        "-"
        (ast/binop
          "*"
          (ast/unop "-" (ast/number 23))
          (ast/unop "+" (ast/number 17)))
        (ast/unop "-" (ast/number 4))))))

(define (g/ast/operators/inc-dec)
  (t/group "increment and decrement"
    (cb/ast
      "increment"
      "$a++ + $b++"
      (ast/binop
        "+"
        (ast/unop "++" ast/$a)
        (ast/unop "++" ast/$b)))
    (cb/ast
      "increment"
      "$a-- - $b--"
      (ast/binop
        "-"
        (ast/unop "--" ast/$a)
        (ast/unop "--" ast/$b)))))

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
                  ("~="  "~")
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

(define (g/ast/operators/compare/string)
  (op-group/binary/left "string compare" "cmp"))
(define (g/ast/operators/compare/number)
  (op-group/binary/left "number compare" "<=>"))
(define (g/ast/operators/smart)
  (op-group/binary/left "smart match" "~~"))

(define (g/ast/operators/concat)
  (op-group/binary/left "concatenate" "~"))

(define (g/ast/operators/low-not)
  (op-group/unary/prefix "low not" "not"))
(define (g/ast/operators/high-not)
  (op-group/unary/prefix "high not" "!"))

(define (g/ast/operators/math)
  (op-group/binary/left "add" "+")
  (op-group/binary/left "add" "-")
  (op-group/binary/left "add" "*")
  (op-group/binary/left "add" "/"))

(define (op-group/method/common test title op m1 c1 m2 c2)
  (t/group
    (text title " common")
    (cb/ast
      "variable invocant, no flags, no arguments"
      (text "$foo" op m1)
      (test #f
        (ast/lexvar "$foo")
        c1))
    (cb/ast
      "grouping invocant, no flags, no arguments"
      (text "($foo + $bar)" op m1)
      (test #f
        (ast/binop "+" (ast/lexvar "$foo") (ast/lexvar "$bar"))
        c1))
    (cb/ast
      "call again on result"
      (text "$foo" op m1 op m2)
      (test #f
        (test #f
          (ast/lexvar "$foo")
          c1)
        c2))
    (cb/ast
      "maybe flag, bareword method, no arguments"
      (text "$foo" op m1 "?")
      (test #t
        (ast/lexvar "$foo")
        c1))
    (cb/ast
      "no flags, bareword method, empty arguments"
      (text "$foo" op m1 "()")
      (test #f
        (ast/lexvar "$foo")
        c1))
    (cb/ast
      "no flags, bareword method, single pos argument"
      (text "$foo" op m1 "(23)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/number 23)))
    (cb/ast
      "no flags, bareword method, single named argument"
      (text "$foo" op m1 "(baz: 23)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/named (ast/string "baz") (ast/number 23))))
    (cb/ast
      "no flags, bareword method, multiple pos arguments"
      (text "$foo" op m1 "(23, 17, 28)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/number 23)
        (ast/number 17)
        (ast/number 28)))
    (cb/ast
      "no flags, bareword method, multiple named arguments"
      (text "$foo" op m1 "(x: 23, y: 17, z: 28)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/named (ast/string "x") (ast/number 23))
        (ast/named (ast/string "y") (ast/number 17))
        (ast/named (ast/string "z") (ast/number 28))))
    (cb/ast
      "no flags, bareword method, dynamically named argument"
      (text "$foo" op m1 "((23 + 17): 42, $bar: 17)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/named
          (ast/binop "+" (ast/number 23) (ast/number 17))
          (ast/number 42))
        (ast/named
          (ast/lexvar "$bar")
          (ast/number 17))))
    (cb/ast
      "no flags, bareword method, mixed arguments"
      (text "$foo" op m1 "(23, 17, x: 42, y: 28)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/number 23)
        (ast/number 17)
        (ast/named (ast/string "x") (ast/number 42))
        (ast/named (ast/string "y") (ast/number 28))))
    (cb/ast
      "maybe flag, bareword method, single argument"
      (text "$foo" op m1 "?(23)")
      (test #t
        (ast/lexvar "$foo")
        c1
        (ast/number 23)))
    (cb/ast
      "maybe flag, bareword methods, single argument called on result"
      (text "$foo" op m1 "?(23)" op m2 "?(17)")
      (test #t
        (test #t
          (ast/lexvar "$foo")
          c1
          (ast/number 23))
        c2
        (ast/number 17)))
    (cb/ast
      "no flags, bareword methods, single argument called on result"
      (text "$foo" op m1 "(23)" op m2 "(17)")
      (test #f
        (test #f
          (ast/lexvar "$foo")
          c1
          (ast/number 23))
        c2
        (ast/number 17)))
    (cb/ast
      "positinoal argument splices"
      (text "$foo" op m1 "(23, @$baz, 17)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/number 23)
        (ast/splice-@ (ast/lexvar "$baz"))
        (ast/number 17)))
    (cb/ast
      "named argument splices"
      (text "$foo" op m1 "(x: 23, %$baz, z: 17)")
      (test #f
        (ast/lexvar "$foo")
        c1
        (ast/named (ast/string "x") (ast/number 23))
        (ast/splice-% (ast/lexvar "$baz"))
        (ast/named (ast/string "z") (ast/number 17))))))

(define (g/ast/operators/method-ref)
  (let ((test (lambda (maybe inv met . args)
                (apply ast/method-ref maybe inv met args))))
    (op-group/method/common
      test
      "fixed method ref"
      ".&"
      "bar" (ast/string "bar")
      "baz" (ast/string "baz"))
    (op-group/method/common
      test
      "variable method ref"
      ".&"
      "$mbar" (ast/lexvar "$mbar")
      "$mbaz" (ast/lexvar "$mbaz"))))

(define (g/ast/operators/method-call)
  (let ((test (lambda (maybe inv met . args)
                (apply ast/method-call maybe #f inv met args))))
    (op-group/method/common
      test
      "fixed method call"
      "."
      "bar" (ast/string "bar")
      "baz" (ast/string "baz"))
    (op-group/method/common
      test
      "variable method call"
      "."
      "$mbar" (ast/lexvar "$mbar")
      "$mbaz" (ast/lexvar "$mbaz")))
  (t/group
    "chained flag"
    (cb/ast
      "chained flag, no arguments"
      (text "$foo.bar!")
      (ast/method-call #f #t
        (ast/lexvar "$foo")
        (ast/string "bar")))
    (cb/ast
      "chained flag, with arguments"
      "$foo.bar!(23)"
      (ast/method-call #f #t
        (ast/lexvar "$foo")
        (ast/string "bar")
        (ast/number 23)))
    (cb/ast
      "could be a high not op"
      "$foo.bar !+23"
      (ast/binop "+"
        (ast/method-call #f #t
          (ast/lexvar "$foo")
          (ast/string "bar"))
        (ast/number 23)))
    (cb/ast
      "multiple chained calls, no arguments"
      "$foo.bar!.baz!"
      (ast/method-call #f #t
        (ast/method-call #f #t
          (ast/lexvar "$foo")
          (ast/string "bar"))
        (ast/string "baz")))
    (cb/ast
      "multiple chained calls, with arguments"
      "$foo.bar!(23).baz!(17)"
      (ast/method-call #f #t
        (ast/method-call #f #t
          (ast/lexvar "$foo")
          (ast/string "bar")
          (ast/number 23))
        (ast/string "baz")
        (ast/number 17)))
    (cb/ast
      "optinoal chained call, no arguments"
      "$foo.bar?!"
      (ast/method-call #t #t
        (ast/lexvar "$foo")
        (ast/string "bar")))
    (cb/ast
      "optional chained call, with arguments"
      "$foo.bar?!(23)"
      (ast/method-call #t #t
        (ast/lexvar "$foo")
        (ast/string "bar")
        (ast/number 23)))))

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

(define (g/ast/hashes)
  (t/group
    "hashes"
    (cb/ast
      "empty hash"
      "{}"
      (ast/hash))
    (cb/ast
      "single element"
      "{ x: 23 }"
      (ast/hash (ast/named (ast/string "x") (ast/number 23))))
    (cb/ast
      "multiple elements"
      "{ x: 23, y: 17, z: 42 }"
      (ast/hash
        (ast/named (ast/string "x") (ast/number 23))
        (ast/named (ast/string "y") (ast/number 17))
        (ast/named (ast/string "z") (ast/number 42))))
    (cb/ast
      "trailing comma"
      "{ x: 23, y: 17, z: 42, }"
      (ast/hash
        (ast/named (ast/string "x") (ast/number 23))
        (ast/named (ast/string "y") (ast/number 17))
        (ast/named (ast/string "z") (ast/number 42))))
    (cb/ast
      "nested"
      "{ x: 23, y: { a: 3, b: 4 }, z: 17 }"
      (ast/hash
        (ast/named (ast/string "x") (ast/number 23))
        (ast/named
          (ast/string "y")
          (ast/hash
            (ast/named (ast/string "a") (ast/number 3))
            (ast/named (ast/string "b") (ast/number 4))))
        (ast/named (ast/string "z") (ast/number 17))))
    (cb/ast
      "spliced"
      "{ x: 2, %$foo, %{ a: 3, b: 4 }, y: 5 }"
      (ast/hash
        (ast/named (ast/string "x") (ast/number 2))
        (ast/splice-% (ast/lexvar "$foo"))
        (ast/splice-%
          (ast/hash
            (ast/named (ast/string "a") (ast/number 3))
            (ast/named (ast/string "b") (ast/number 4))))
        (ast/named (ast/string "y") (ast/number 5))))))

(define (g/ast/arrays)
  (t/group
    "arrays"
    (cb/ast
      "empty array"
      "[]"
      (ast/array))
    (cb/ast
      "single element"
      "[23]"
      (ast/array (ast/number 23)))
    (cb/ast
      "multiple elements"
      "[2, 3, 4]"
      (ast/array (ast/number 2) (ast/number 3) (ast/number 4)))
    (cb/ast
      "trailing comma"
      "[2, 3, 4,]"
      (ast/array (ast/number 2) (ast/number 3) (ast/number 4)))
    (cb/ast
      "nested"
      "[2, [3, 4], 5]"
      (ast/array
        (ast/number 2)
        (ast/array (ast/number 3) (ast/number 4))
        (ast/number 5)))
    (cb/ast
      "spliced"
      "[2, @$foo, @[3, 4], 5]"
      (ast/array
        (ast/number 2)
        (ast/splice-@ (ast/lexvar "$foo"))
        (ast/splice-@ (ast/array (ast/number 3) (ast/number 4)))
        (ast/number 5)))))

(define (g/ast/function-calls)
  (t/group
    "function calls"
    (cb/ast
      "simple static call without arguments"
      "foo()"
      (ast/func-call "foo"))
    (cb/ast
      "simple static call with arguments"
      "foo(23, 17)"
      (ast/func-call "foo" (ast/number 23) (ast/number 17)))
    (cb/ast
      "variable function call"
      "$foo(23, 17)"
      (ast/call (ast/lexvar "$foo") (ast/number 23) (ast/number 17)))
    (cb/ast
      "calling grouping result"
      "($foo)($bar)"
      (ast/call (ast/lexvar "$foo") (ast/lexvar "$bar")))
    (cb/ast
      "calling method call return value"
      "$foo.bar(23)(17)"
      (ast/call
        (ast/method-call #f #f
          (ast/lexvar "$foo")
          (ast/string "bar")
          (ast/number 23))
        (ast/number 17)))))

(define (g/ast/strings)
  (t/group
    "strings"
    (cb/ast
      "single quoted, simple"
      "'foo'"
      (ast/string "foo"))
    (cb/ast
      "single quoted, escaped quote"
      "'foo \\' bar'"
      (ast/string "foo ' bar"))
    (cb/ast
      "single quoted, newline and tab"
      "'foo \\n\\t bar'"
      (ast/string "foo \\n\\t bar"))
    (cb/ast
      "single quoted, concat"
      "'foo' ~ 'bar'"
      (ast/binop "~"
        (ast/string "foo")
        (ast/string "bar")))
    (cb/ast
      "single quoted multiline"
      "'foo\nbar'"
      (ast/string "foo\nbar"))
    (cb/ast
      "double quoted, simple"
      "\"foo\""
      (ast/string "foo"))
    (cb/ast
      "double quoted, escaped quote"
      "\"foo \\\" bar\""
      (ast/string "foo \" bar"))
    (cb/ast
      "double quoted, newline and tab"
      "\"foo \\n\\t bar\""
      (ast/string "foo \n\t bar"))
    (cb/ast
      "double quoted, concat"
      "\"foo\" ~ \"bar\""
      (ast/binop "~"
        (ast/string "foo")
        (ast/string "bar")))
    (cb/ast
      "double quoted multiline"
      "\"foo\nbar\""
      (ast/string "foo\nbar"))))

(define (g/ast/slots)
  (t/group
    "slots"
    (cb/ast
      "simple access"
      "$foo[23]"
      (ast/slot-ref (ast/lexvar "$foo") (ast/number 23)))
    (cb/ast
      "nested access"
      "$foo[23][-5]"
      (ast/slot-ref
        (ast/slot-ref (ast/lexvar "$foo") (ast/number 23))
        (ast/unop "-" (ast/number 5))))
    (cb/ast
      "assignment"
      "$foo[23] = 17"
      (ast/assign
        (ast/slot-ref (ast/lexvar "$foo") (ast/number 23))
        (ast/number 17)))
    (cb/ast
      "nested assignment"
      "$foo[23][-1] = 17"
      (ast/assign
        (ast/slot-ref
          (ast/slot-ref (ast/lexvar "$foo") (ast/number 23))
          (ast/unop "-" (ast/number 1)))
        (ast/number 17)))))

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
    g/ast/operators/high-not
    g/ast/operators/equality
    g/ast/operators/compare/string
    g/ast/operators/compare/number
    g/ast/operators/smart
    g/ast/operators/concat
    g/ast/operators/math
    g/ast/operators/num-signs
    g/ast/operators/method-call
    g/ast/operators/method-ref))

(define (g/ast)
  (t/group
    "ast"
    g/ast/numbers
    g/ast/document
    g/ast/operators
    g/ast/variables
    g/ast/groupings
    g/ast/arrays
    g/ast/hashes
    g/ast/slots
    g/ast/function-calls
    g/ast/strings))

(g/ast)

(t/done)
