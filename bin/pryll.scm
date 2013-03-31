(declare (uses cmd/help
               cmd/build))

(define commands
  `(("help"     . ,cmd/help-commands)
    ("build"    . ,cmd/build)))

(define (main full-args)
  (let* ((program (car full-args))
         (args (cdr full-args)))
    (cond ((null? args)
           (cmd/help-commands program '()))
          (else
            (let* ((cmd (car args))
                   (handler (assoc cmd commands)))
              (if handler
                ((cdr handler) program (cdr args))
                (begin
                  (printf "Unknown command '~a'. See `~a help`\n"
                          cmd
                          program)
                  (exit))))))))

(main (argv))
