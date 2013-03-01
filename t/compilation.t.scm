(require-extension srfi-1 srfi-13)

(load "lib/ast.scm")
(load "lib/parser.scm")
(load "lib/test.scm")
(load "lib/util.scm")
(load "lib/compiler.scm")

(import pryll/parsing)
(import pryll/ast)
(import pryll/test)
(import pryll/util)
(import pryll/compiler)
(import chicken scheme)

(define (cb/run title source result-test)
  (lambda ()
    (t/group
      title
      (lambda ()
        (let* ((ast  (source->ast title source))
               (code (ast->code ast)))
          (result-test (eval code (scheme-report-environment 5))))))))

(define (t/pred title pred value)
  (t/ok (pred value) title))

(define (cb/number num)
  (cb/group
    "number identity"
    (lambda (value)
      (and (t/pred "is a number" number? value)
           (is-equal = value num "numeric value")))))

(t/group
  "numbers"
  (cb/run "simple integer"
          "23"
          (cb/number 23))
  (cb/run "zero integer"
          "0"
          (cb/number 0))
  (cb/run "float"
          "23.5"
          (cb/number 23.5)))

(t/group
  "documents"
  (cb/run "multiple statements"
          "23; 5; 17"
          (cb/number 17)))

(t/done)
