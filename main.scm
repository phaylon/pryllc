
(load "ast")
(load "parser.scm")
(import parsing)
(import ast)

(define ast (source->ast "test" "not 23 or 17"))
(define (say . ls) (map display ls) (newline))

(pretty-print (debug-dump ast))
