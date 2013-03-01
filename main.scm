(load "lib/ast.scm")
(load "lib/parser.scm")
(load "lib/objects.scm")
(load "lib/util.scm")
(load "lib/compiler.scm")

(import pryll/objects)
(import pryll/parsing)
(import pryll/ast)
(import pryll/util)
(import pryll/compiler)

(define source
  (list-ref (argv) (- (length (argv)) 1)))

(define ast (source->ast "command line" source))
(define (say . ls) (map display ls) (newline))

(pretty-print (pryll:call-method ast "debug-dump" (list) (mkhash)))

(define code (ast->code ast))

(pretty-print code)

(define result (eval code (scheme-report-environment 5)))

(say "RESULT")
(pretty-print result)
