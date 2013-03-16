(declare (uses util))
(declare (uses mop))
(declare (uses ast))
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
    ))

(require-extension extras posix utils)
(import chicken scheme)

(define (get-pryll-root)
  "./")

(define (compile-pryll-expression options args)
  (let* ((expr (cli/option-value options 'eval))
         (ast  (source->ast "command line expression" expr))
         (code (ast->code ast))
         (out  (if (cli/option-exists? options 'output)
                 (cli/option-value options 'output)
                 #f))
         (rm   (not out))
         (name (or out (conc ".pryll." (current-process-id))))
         (scm  (conc name ".scm"))
         (scmo (conc name ".o")))
;    (say "CODE")
;    (say code)
;    (say "---")
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

(define (compile-pryll-file options args)
  (say "NYI"))

(cli/parse
  `(((eval e)     s "Evaluate expression")
    ((help h ?)   b "Display help")
    ((run r)      b "Run program after compilation")
    ((output o)   s "Output to file (defaults to pryll.out"))
  (lambda (opt args)
    (cond ((cli/option-exists? opt 'eval)
           (compile-pryll-expression opt args))
          (else
           (compile-pryll-file opt args)))))
