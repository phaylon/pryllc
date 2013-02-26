
(module objects
  ()
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
          (lambda (pos nam)
            (let* ((self   (car pos))
                   (object (cadr pos))
                   (name   (get-slot self "name")))
              (get-slot object name))))
        (mkmethod
          "set-value"
          (lambda (pos nam)
            (let* ((self      (car pos))
                   (object    (cadr pos))
                   (name      (get-slot self "name"))
                   (new-value (caddr pos)))
              (set-slot! object name new-value))))
        )))

  (dbg "creating meta method")
  (set! pryll:meta-method
    (mkclass
      (named->hash
        (mkattr "name"))
      (named->hash
        (mkmethod
          "execute"
          (lambda (pos nam)
            (let* ((self (car pos))
                   (object (cadr pos))
                   (restpos (caddr pos))
                   (method (get-slot self "body")))
              (method object restpos nam))))
        )))

  (dbg "creating meta class")
  (set! pryll:meta-class
    (mkclass
      (named->hash
        (mkattr "roles")
        (mkattr "name")
        (mkattr "superclasses")
        (mkattr "attributes")
        (mkattr "methods"))
      (named->hash
        (mkmethod
          "new"
          (lambda (pos nam)
            (make-object
              (car pos)
              nam)))
        (mkmethod
          "find-method"
          (lambda (pos nam)
            (car (hash-table-ref
                   (get-slot (car pos) "methods")
                   (cadr pos)))))
        (mkmethod
          "execute-method"
          (lambda (pos nam)
            (let* ((self (car pos))
                   (met (cadr pos))
                   (obj (caddr pos))
                   (restpos (cdddr pos)))
              (let* ((methods (get-slot self "methods"))
                     (method (car (hash-table-ref methods met)))
                     (func (get-slot method "body")))
                (func (append (list obj) restpos) nam)))))
        )))

  (define pryll:meta-role
    (mkclass
      (named->hash
        (mkattr "name")
        (mkattr "roles")
        (mkattr "attributes")
        (mkattr "methods"))
      (mkhash)))

  (define (pryll:call-method-static object name pos_args named_args)
    (dbg "realised call: " name)
    (let* ((meta (force (object-meta object)))
           (methods (get-slot meta "methods"))
           (method (car (hash-table-ref methods name)))
           (code (get-slot method "body")))
      (code (append (list object) pos_args) named_args)))

  (define (pryll:call-method object name positional named)
    (dbg "method call: " name)
    (let* ((meta (force (object-meta object))))
      (if (eqv? object meta)
        (pryll:call-method-static meta name positional named)
        (pryll:call-method
          meta
          "execute-method"
          (append
            (list name object)
            positional)
          named))))

  (define (pryll:new class . args)
    (pryll:call-method
      class
      "new"
      (list)
      (apply mkhash args)))

;;
;;  TESTING
;;

  (define test-class
    (pryll:new pryll:meta-class
               (list "attributes"
                     (named->hash
                       (mkattr "label")))
               (list "methods"
                     (named->hash
                       (mkmethod
                         "label"
                         (lambda (pos nam)
                           (get-slot (car pos) "label")))))))

  (define test-object
    (pryll:new
      test-class 
      (list "label" "Foo")))

  (dbg "Label: " (pryll:call-method test-object "label" (list) (mkhash)))
)
