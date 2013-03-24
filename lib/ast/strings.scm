(declare (unit ast/strings))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(define <pryll:ast-string>
  (mop/init
    (mop/class name: "Core::AST::String")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "value"))
      (call add-methods:
            (compile-method
              (lambda (self ctx)
                (pryll:invoke self "value")))
            (dump-method
              (lambda (self)
                `(str ,(pryll:invoke self "value")))))
      (call finalize:))))

(define-inline (trim-string str chr)
  (irregex-replace
    `(: bos ,chr)
    (irregex-replace
      `(: ,chr eos)
      str
      "")
    ""))

(define (make-string-value token)
  (pryll:make <pryll:ast-string>
              location:   (token-location token)
              value:      (token-value token)))

(define (make-string-single token)
  (pryll:make <pryll:ast-string>
              location:   (token-location token)
              value:      (irregex-replace/all
                            '(: "\\'")
                            (trim-string (token-value token) "'")
                            "'")))

(define-inline (prepare-string-double-chars str)
  (define replace-map
    '(("n"  "\n")
      ("t"  "\t")
      ("\"" "\"")))
  (irregex-replace/all
    '(: #\\ ($ any))
    str
    (lambda (match)
      (let ((p (assoc (irregex-match-substring match 1) replace-map)))
        (if p
          (cadr p)
          (error "Invalid escaped char"))))))

(define (make-string-double token)
  (pryll:make <pryll:ast-string>
              location:   (token-location token)
              value:      (prepare-string-double-chars
                            (trim-string (token-value token) "\""))))

(define (identifier->string ident)
  (pryll:make <pryll:ast-string>
              location:   (pryll:invoke ident "location")
              value:      (pryll:invoke ident "value")))

