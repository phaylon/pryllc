(declare (uses util))
(declare (uses mop))
(declare (uses parser))
(declare (uses compiler))
(declare (uses exceptions))
(declare (uses errors))
(declare (uses cli))
;(declare (uses build))

(define runtime-uses
  '(util
    mop
    exceptions
    errors
    stack
    meta/number
    meta/string
    meta/array
    meta/hash
    meta/void
    nether
    primitives
    runtime
    ))

(require-extension extras posix utils)
(import chicken scheme)

(define (get-pryll-root)
  "./")

(define (generate-ast options source-name source-body)
  (let ((ast (source->ast source-name source-body)))
    (if (cli/option-exists? options 'dump-ast)
      (begin
        (pretty-print (pryll:invoke ast "debug-dump"))
        (exit))
      ast)))

(define (generate-code options ast)
  (let ((code (ast->code ast)))
    (if (cli/option-exists? options 'dump-scheme)
      (begin
        (pretty-print code)
        (exit))
      code)))

(define (compile-pryll-code options src-code src-name args)
  (let* ((expr src-code)
         (ast  (generate-ast options src-name expr))
         (code (generate-code options ast))
         (out  (if (cli/option-exists? options 'output)
                 (cli/option-value options 'output)
                 #f))
         (rm   (not out))
         (name (or out (conc ".pryll." (current-process-id))))
         (scm  (conc name ".scm"))
         (scmo (conc name ".o")))
    (with-output-to-file
      scm
      (lambda ()
        (write `(declare (uses ,@runtime-uses)))
        (and rm
             (write `(on-exit
                       (lambda ()
                         (delete-file ,name)))))
        (write `(let ((result ,code))
                  (display "-> ")
                  (display result)
                  (newline)))
        ))
    (system* "csc -c ~a -o ~a"
             (qs scm)
             (qs scmo))
    (let ((objs (string-join
                  (append
                    (map (lambda (lib)
                           (conc (get-pryll-root)
                                 "lib/"
                                 (symbol->string lib)
                                 ".o"))
                         runtime-uses)
                    (list scmo))
                  " ")))
      (system* "csc ~a -o ~a"
               objs
               (qs name)))
    (delete-file scm)
    (delete-file scmo)
    (process-execute (conc "./" name) args)
    ))

(define (compile-pryll-expression options args)
  (let ((expr (cli/option-value options 'eval)))
    (compile-pryll-code
      options
      expr
      "command line expression"
      args)))

(define (compile-pryll-file options args)
  (compile-pryll-code
    options
    (read-all (car args))
    (car args)
    (cdr args)))

(cli/parse
  (argv)
  `(((eval e)       s "Evaluate expression")
    ((dump-ast)     b "Dump the generated AST")
    ((dump-scheme)  b "Dump the generated Scheme code")
    ((help h ?)     b "Display help")
    ((run r)        b "Run program after compilation")
    ((output o)     s "Output to file (defaults to pryll.out"))
  (lambda (opt args)
    (cond ((cli/option-exists? opt 'eval)
           (compile-pryll-expression opt args))
          (else
           (compile-pryll-file opt args)))))
