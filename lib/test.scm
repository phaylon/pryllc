(load "lib/ast.scm")
(load "lib/parser.scm")
(load "lib/util.scm")
(load "lib/objects.scm")

(module pryll/test
  (t/eq
   t/class
   t/done
   t/indent
   t/pass
   t/fail
   t/ok
   t/diag
   t/true-list
   t/group
   cb/group
   is-object
   is-equal
   test-all
   cb/is
   cb/list
   cb/object
   cb/slot)
  (import chicken scheme)
  (import pryll/objects)
  (import pryll/util)
  (import pryll/parsing)
  (import pryll/ast)
  (require-extension srfi-1 srfi-13 srfi-69 data-structures)

  (define t/level 0)
  (define t/fail-cnt 0)
  (define t/pass-cnt 0)

  (define (t/eq a b) (equal? (->string a) (->string b)))

  (define (t/class class)
    (string-concatenate
      (list
        "is a "
        "(Class Names NYI)")))

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
    (or (and (t/ok (pryll:isa? obj class)
                   (t/class class))
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
        (text slot " slot")
        (lambda ()
          (test (pryll:get-slot obj (symbol->string slot)))))))

)
