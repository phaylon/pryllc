(declare (unit exceptions))
(declare (export pryll:throw))

(import chicken scheme)
(require-extension srfi-1 srfi-13 data-structures)

(define (top-handler ex)
  (display
    (apply conc
           "Caught "
           (conc "<"
                 (pryll:name (pryll:meta-for ex))
                 ">")
           (let ((msg (pryll:invoke
                        ex
                        "get-full-message"
                        #f
                        #f
                        (lambda () #f))))
             (if msg
               (conc ": " msg)
               ""))
           '()))
  (newline)
  (exit 255))

(define current-handler (make-parameter top-handler))

(define (pryll:throw ex)
  ((current-handler) ex))

