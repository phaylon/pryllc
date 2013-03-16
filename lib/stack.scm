(declare (unit stack))

(import chicken scheme)
(require-extension srfi-1 srfi-13 srfi-69 data-structures)

(define pryll:stack '(("top-level")))

(define (pryll:stack-level id proc)
  (dynamic-wind
    (lambda () (set! pryll:stack (cons id pryll:stack)))
    proc
    (lambda () (set! pryll:stack (cdr pryll:stack)))))

(define (pryll:current-stack)
  (apply list pryll:stack))
