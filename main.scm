
(load "lib/ast.scm")
(load "lib/parser.scm")
(load "lib/objects.scm")
(load "lib/util.scm")
(import pryll/objects)
(import pryll/parsing)
(import pryll/ast)
(import pryll/util)

(define source
  (list-ref (argv) (- (length (argv)) 1)))

;(define source (cadddr (ar))

(define ast (source->ast "command line" source))
(define (say . ls) (map display ls) (newline))

(pretty-print (pryll:call-method ast "debug-dump" (list) (mkhash)))
