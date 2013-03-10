
(module
  pryll/exceptions
  (pryll:throw)

  (import chicken scheme)
  (require-extension srfi-1 srfi-13 data-structures)

  (define (top-handler ex)
    (error "An error occured" ex))

  (define current-handler (make-parameter top-handler))

  (define (pryll:throw ex)
    ((current-handler) ex))

  (pryll:throw 23)
)
