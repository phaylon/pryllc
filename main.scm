(include "lib/util.scm")
(include "lib/mop.scm")
(include "lib/ast.scm")
(include "lib/parser.scm")
(include "lib/compiler.scm")

(import pryll/mop)
(import pryll/parsing)
(import pryll/ast)
(import pryll/util)
(import pryll/compiler)

(define source
  (list-ref (argv) (- (length (argv)) 1)))

(define ast (source->ast "command line" source))
(define (say . ls) (map display ls) (newline))

(pretty-print (pryll:invoke ast "debug-dump"))

(define code (ast->code ast))

(pretty-print code)

(define result (eval code (scheme-report-environment 5)))

(say "RESULT")
(pretty-print result)
