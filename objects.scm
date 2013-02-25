
(module objects
  (pryll:call-method)
  (import chicken scheme)
  (require-extension srfi-1 srfi-13 srfi-69)

  (define DEBUG #t)

  (define (dbg . items)
    (if DEBUG
      (begin
        (display "[debug] ")
        (map display items)
        (newline))))

  (define-inline (mkhash . alist)
    (dbg "make hash " alist)
    (alist->hash-table
      alist
      test: string=?
      hash: string-hash
      size: 32))

  (define-record object meta data)

  (define (get-slot obj slot)
    (car (hash-table-ref (object-data obj) slot)))
  (define (set-slot! obj slot value)
    (hash-table-set! (object-data obj) slot value))

  (define (named->hash . items)
    (dbg "transform " items)
    (apply
      mkhash
      (map (lambda (item)
             (dbg "to pair " item)
             (list (get-slot item "name")
                   item))
           items)))

  (define pryll:meta-class)
  (define pryll:meta-attribute)
  (define pryll:meta-method)

  (define (mkclass attrs methods)
    (dbg "make class")
    (make-object
      (delay pryll:meta-class)
      (mkhash
        (list "attributes" attrs)
        (list "methods" methods))))

  (define (mkmethod name body)
    (dbg "make method " name)
    (make-object
      (delay pryll:meta-method)
      (mkhash
        (list "name" name)
        (list "body" body))))

  (define (mkattr name)
    (dbg "make attribute " name)
    (make-object
      (delay pryll:meta-attribute)
      (mkhash
        (list "name" name))))

  (dbg "create meta attribute")
  (set! pryll:meta-attribute
    (mkclass
      (named->hash
        (mkattr "name"))
      (named->hash
        (mkmethod
          "get-value"
          (lambda (self pos nam)
            (get-slot (car pos) (get-slot self "name"))))
        (mkmethod
          "set-value"
          (lambda (self pos nam)
            (set-slot! (car pos) (get-slot self "name") (cadr pos))))
        )))

  (dbg "creating meta method")
  (set! pryll:meta-method
    (mkclass
      (named->hash
        (mkattr "name")
        (mkattr "body"))
      (named->hash
        (mkmethod
          "execute"
          (lambda (self pos nam)
            (let* ((object (car pos))
                   (method (get-slot self "body")))
              (method object (cdr pos) nam))))
        )))

  (dbg "creating meta class")
  (set! pryll:meta-class
    (mkclass
      (named->hash
        (mkattr "name")
        (mkattr "superclasses")
        (mkattr "attributes")
        (mkattr "methods"))
      (named->hash
        (mkmethod
          "new"
          (lambda (self pos nam)
            (make-object
              self
              nam)))
        (mkmethod
          "find-method"
          (lambda (self pos nam)
            (car (hash-table-ref (get-slot self "methods") (car pos)))))
        (mkmethod
          "execute-method"
          (lambda (self pos nam)
            (let* ((met (car pos))
                   (obj (cadr pos)))
              (let* ((methods (car (get-slot self "methods")))
                     (method (hash-table-ref methods (car pos)))
                     (func (get-slot method "body")))
                (func obj (cddr pos) nam)))))
        )))

  (define (pryll:call-method object name pos_args named_args)
    (dbg "method call: " name)
    (let* ((meta (force (object-meta object)))
           (methods (get-slot meta "methods"))
           (method (car (hash-table-ref methods name)))
           (code (get-slot method "body")))
      (code object pos_args named_args)))

;;
;;  TESTING
;;

  (define test-class
    (pryll:call-method
      pryll:meta-class
      "new"
      (list)
      (mkhash
        (list
          "attributes"
          (named->hash
            (mkattr "label")))
        (list
          "methods"
          (named->hash
            (mkmethod
              "label"
              (lambda (self pos nam)
                (get-slot self "label"))))))))

  (define test-object
    (pryll:call-method
      test-class
      "new"
      (list)
      (mkhash (list "label" "Foo"))))

  (dbg "object: " test-object)
  (dbg "label: " (pryll:call-method test-object "label" (list) (mkhash)))
)
