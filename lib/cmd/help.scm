(declare (unit cmd/help))

(require-extension srfi-13 data-structures)
(import chicken scheme)

(define (help/help program)
  (print-lines
    (usage program "help" "[<command>]")
    ""
    "Shows generic or command specific help information."
    ""
    command-list))

(define (help/build program)
  (print-lines
    (usage program "build")
    ""
    "Builds the entire project tree."))

(define command-help
  `(("help"     . ,help/help)
    ("build"    . ,help/build)))

(define (print-lines . ls)
  (for-each (lambda (line)
              (if (list? line)
                (apply print-lines line)
                (begin
                  (display line)
                  (newline))))
            ls))

(define command-list
  (list "Commands:"
        "  build            Build the project"
        "  help             Show general help information"
        "  help <command>   Show command specific help information"
        ))

(define (usage program #!optional cmd rest)
  (conc "Usage: "
        program
        " "
        (or cmd "<command>")
        " "
        (or rest "")))

(define (cmd/help-commands program args)
  (if (null? args)
    (print-lines
      (usage program #f "<options> <arguments>")
      ""
      command-list)
    (let ((cmd (assoc (car args) command-help)))
      (if cmd
        ((cdr cmd) program)
        (begin
          (printf "Unknown command '~a' has no help information."
                  (car args))
          (exit))))))
