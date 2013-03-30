(declare (unit nether))
(declare (uses mop util errors))

(declare (hide nether-methods))
(define nether-methods
  `(("meta"     . nether/meta)
    ("array"    . nether/array)
    ("gist"     . nether/gist)
    ))

(define (nether-method name)
  (let ((found (assoc name nether-methods)))
    (if found
      (cdr found)
      #f)))

(define-inline (gist-list ls)
  (string-join (map (lambda (item)
                      (nether/gist (list item) (mkhash)))
                    ls)
               ", "))

(define-inline (namify value)
  (if (v-true? value)
    value
    "<anon>"))

(define (nether/gist pos nam)
  (define value (car pos))
  (cond ((boolean? value)
         (if value "true" "false"))
        ((number? value)
         (number->string value))
        ((string? value)
         (sprintf "~s" value))
        ((void? value)
         "undef")
        ((list? value)
         (conc "[" (gist-list value) "]"))
        (else
          (let ((meta (pryll:meta-for value)))
            (cond ((pryll:isa? value <pryll:meta-module>)
                   (sprintf "#(module ~a)"
                            (namify (pryll:invoke value "name"))))
                  ((pryll:isa? value <pryll:meta-class>)
                   (sprintf "#(class ~a)"
                            (namify (pryll:invoke value "name"))))
                  (else
                    (sprintf "#(instance of ~a)"
                             (namify (pryll:invoke meta "name")))))))))

(define (nether/array pos nam)
  (let ((value (car pos)))
    (if (list? value)
      value
      (list value))))

(define (nether/meta pos nam)
  (pryll:meta-for (car pos)))
