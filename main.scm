(require-extension coops)

(load "ast")
(load "parser.scm")
(import parsing)
(import ast)

(define ast (source->ast "test" "23;17"))
(define (say . ls) (map display ls) (newline))

(say ast)
(say (debug-dump ast))
