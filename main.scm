
(load "ast")
(load "parser.scm")
(import parsing)
(import ast)

(define source (cadddr (argv)))

(define ast (source->ast "command line" source))
(define (say . ls) (map display ls) (newline))

(pretty-print (debug-dump ast))
