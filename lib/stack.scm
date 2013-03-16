(declare (unit stack))

(import chicken scheme)
(require-extension srfi-1 srfi-13 srfi-69 data-structures)

(define (pryll:stack-id type location description)
  (list type location description))

(define (pryll:format-stack-id id)
  (conc (list-ref id 0)
        (let ((desc (list-ref id 2)))
          (if desc
            (conc " " desc)
            ""))
        (let ((loc (list-ref id 1)))
          (if loc
            (conc "\n\t\tin "
                  (list-ref loc 0)
                  " line "
                  (list-ref loc 1)
                  " char "
                  (list-ref loc 2)
                  "")
            ""))))

(define pryll:stack
  (list (pryll:stack-id "top-level" #f #f)))

(define (pryll:stack-level id proc)
  (dynamic-wind
    (lambda () (set! pryll:stack (cons id pryll:stack)))
    proc
    (lambda () (set! pryll:stack (cdr pryll:stack)))))

(define (pryll:current-stack)
  (apply list pryll:stack))
