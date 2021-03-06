(require-extension srfi-1 srfi-13)

(load "dev/libs.scm")
(load "t/lib/test.scm")
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

(define (cb/string str)
  (cb/group
    "string identity"
    (lambda (value)
      (and (t/pred "is a string" string? value)
           (is-equal string=? value str "string value")))))

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
  "strings"
  (cb/run "simple string"
          "'foo bar'"
          (cb/string "foo bar")))

(t/group
  "documents"
  (cb/run "multiple statements"
          "23; 5; 17"
          (cb/number 17)))

(t/done)
