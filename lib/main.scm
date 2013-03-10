
(declare (uses util))
(declare (uses mop))
(declare (uses ast))
(declare (uses parser))
(declare (uses compiler))

(require-extension extras)
(import chicken scheme)

(define source
  (list-ref (argv) (- (length (argv)) 1)))

(define ast (source->ast "command line" source))
(define (say . ls) (map display ls) (newline))

(pretty-print (pryll:invoke ast "debug-dump"))

(define code (ast->code ast))

(pretty-print code)

(define result (eval code (scheme-report-environment 5)))

(say ";; RESULT")
(pretty-print result)
