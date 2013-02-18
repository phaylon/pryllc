
(module ast *
  (import chicken scheme)
  (require-extension coops)

  (define (token-value token) (car token))
  (define (token-location token) (cadr token))

;;
;; integer
;;

  (define-class <ast-integer> ()
    ((location)
     (value)))

  (define-method (debug-dump (int <ast-integer>))
    `(int (value ,(slot-value int 'value))))

;;
;; statement
;;

  (define-class <ast-statement> ()
    ((expression)))

  (define-method (debug-dump (stmt <ast-statement>))
    `(stmt ,(debug-dump (slot-value stmt 'expression))))

;;
;; document
;;

  (define-class <ast-document> ()
    ((statements)))

  (define-method (debug-dump (doc <ast-document>))
    `(doc ,(map debug-dump (slot-value doc 'statements))))

;;
;; generators
;;

  (define (make-integer token)
    (make <ast-integer>
      'value (token-value token)
      'location (token-location token)))

  (define (make-statement expression)
    (make <ast-statement>
      'expression expression))

  (define (make-document statements)
    (make <ast-document>
      'statements statements)))
