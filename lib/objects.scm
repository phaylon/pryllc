(load "lib/util.scm")

(module pryll/objects
  (<pryll:meta-class>
   pryll:new
   pryll:make
   pryll:call-method
   pryll:mkattr
   pryll:mkmethod
   pryll:attribute
   pryll:attribute/item
   pryll:get-slot
   pryll:set-slot!
   pryll:meta
   pryll:isa?
   pryll:object?
   named->hash)

  (import chicken scheme)
  (import pryll/util)
  (require-extension srfi-1 srfi-13 srfi-69)

  (define-record object meta data internal)
  (define-record undef)

  (define pryll:undef (make-undef))

  (define ... #f)

  (define (pryll:meta item)
    (if (object? item)
      (object-meta item)
      (pryll:box item)))

  (define (pryll:object? item)
    (object? item))

  (define (pryll:isa? item class)
    (if (pryll:object? item)
      (eqv? (pryll:meta item) class)
      #f))

  (define (pryll:object meta data . rest)
    (let ((internal (if (null? rest) #f (car rest))))
      (make-object meta data internal)))

  (define (named->hash . items)
    (dbg "transform " items)
    (apply
      mkhash
      (map (lambda (item)
             (dbg "to pair " item)
             (list (pryll:get-slot item "name")
                   item))
           items)))

  (define (pryll:get-slot obj slot)
    (dbg "hash-table: " (hash-table->alist (object-data obj)))
    (car (hash-table-ref
           (object-data obj)
           slot)))

  (define (pryll:set-slot! obj slot value)
    (hash-table-set!
      (object-data obj)
      slot
      value))

  (define (pryll:get-internal obj)
    (object-internal obj))

  (define <pryll:meta-class>)
  (define <pryll:meta-attribute>)
  (define <pryll:meta-method>)
  (define <pryll:meta-role>)
  (define <pryll:meta-array>)
  (define <pryll:meta-hash>)

  (define (pryll:box item)
    (cond ((list? item)
           (pryll:array item))
          ((hash-table? item)
           (pryll:hash item))
          ((object? item)
           item)
          (else
           (error "Unable to box" item))))

  (define (pryll:hash hash)
    (pryll:object
      (delay <pryll:meta-hash>)
      (mkhash)
      hash))

  (define (pryll:array ls)
    (pryll:object
      (delay <pryll:meta-array>)
      (mkhash)
      ls))

  (define (mkclass attrs methods)
    (dbg "make class")
    (pryll:object
      (delay <pryll:meta-class>)
      (mkhash
        (list "attributes" attrs)
        (list "methods" methods))))

  (define (pryll:mkmethod name body)
    (dbg "make method " name)
    (pryll:object
      (delay <pryll:meta-method>)
      (mkhash
        (list "name" name))
      body))

  (define (pryll:mkattr name)
    (dbg "make attribute " name)
    (pryll:object
      (delay <pryll:meta-attribute>)
      (mkhash
        (list "name" name))))

  (define (pryll:attribute . args)
    (pryll:object
      (delay <pryll:meta-attribute>)
      (apply mkhash (list->alist args))))

  (define (pryll:attribute/item name)
    (pryll:attribute
      name:             name
      reader:           pryll:undef
      writer:           name
      init-arg:         name
      associated-class: pryll:undef
      is-finalized:     #t))

  (dbg "create meta attribute")
  (set! <pryll:meta-attribute>
    (mkclass
      (named->hash
        (pryll:attribute/item "name")
        (pryll:attribute/item "is-finalized")
        (pryll:attribute/item "associated-class")
        (pryll:attribute/item "init-arg")
        (pryll:attribute/item "reader")
        (pryll:attribute/item "writer"))
      (named->hash
        (pryll:mkmethod
          "get-value"
          (lambda (pos nam)
            (let* ((self   (car pos))
                   (object (cadr pos))
                   (name   (pryll:get-slot self "name")))
              (pryll:get-slot object name))))
        (pryll:mkmethod
          "set-value"
          (lambda (pos nam)
            (let* ((self      (car pos))
                   (object    (cadr pos))
                   (name      (pryll:get-slot self "name"))
                   (new-value (caddr pos)))
              (pryll:set-slot! object name new-value))))
        )))

  (dbg "creating meta method")
  (set! <pryll:meta-method>
    (mkclass
      (named->hash
        (pryll:attribute/item "name")
        (pryll:attribute/item "associated-class"))
      (named->hash
        (pryll:mkmethod
          "execute"
          (lambda (pos nam)
            (let* ((self (car pos))
                   (object (cadr pos))
                   (restpos (caddr pos))
                   (method (pryll:get-internal self)))
              (method object restpos nam))))
        )))

  (dbg "creating meta class")
  (set! <pryll:meta-class>
    (mkclass
      (named->hash
        (pryll:attribute/item "roles")
        (pryll:attribute/item "name")
        (pryll:attribute/item "superclasses")
        (pryll:attribute/item "attributes")
        (pryll:attribute/item "methods"))
      (named->hash
        (pryll:mkmethod
          "new"
          (lambda (pos nam)
            (pryll:object
              (car pos)
              nam)))
        (pryll:mkmethod
          "has-method"
          (lambda (pos nam)
            (let* ((self (car pos))
                   (name (cadr pos)))
              ...)))
        (pryll:mkmethod
          "add-method"
          (lambda (pos nam)
            (let* ((self (car pos))
                   (method (cadr pos)))
              ...)))
        (pryll:mkmethod
          "find-method"
          (lambda (pos nam)
            (car (hash-table-ref
                   (pryll:get-slot (car pos) "methods")
                   (cadr pos)))))
        (pryll:mkmethod
          "execute-method"
          (lambda (pos nam)
            (let* ((self (car pos))
                   (met (cadr pos))
                   (obj (caddr pos))
                   (restpos (cdddr pos)))
              (let* ((methods (pryll:get-slot self "methods"))
                     (method (car (hash-table-ref methods met)))
                     (func (pryll:get-internal method)))
                (func (append (list obj) restpos) nam)))))
        )))

  (set! <pryll:meta-hash>
    (mkclass
      (mkhash)
      (named->hash
        (pryll:mkmethod
          "get"
          (lambda (pos nam)
            (car (hash-table-ref/default
                   (object-internal (car pos))
                   (cadr pos)
                   (pryll:undef)))))
        (pryll:mkmethod
          "set"
          (lambda (pos nam)
            (hash-table-set!
              (object-internal (car pos))
              (cadr pos)
              (caddr pos))
            (caddr pos)))
        )))

  (set! <pryll:meta-array>
    (mkclass
      (mkhash)
      (named->hash
        (pryll:mkmethod
          "get"
          (lambda (pos nam)
            ;; TODO check argument and list size
            (list-ref
              (object-internal (car pos))
              (cadr pos))))
        )))

  (set! <pryll:meta-role>
    (mkclass
      (named->hash
        (pryll:attribute/item "name")
        (pryll:attribute/item "roles")
        (pryll:attribute/item "attributes")
        (pryll:attribute/item "methods"))
      (named->hash)))

  (define (pryll:call-method-static object name pos_args named_args)
    (dbg "realised call: " name)
    (let* ((meta (force (object-meta object)))
           (methods (pryll:get-slot meta "methods"))
           (method (car (hash-table-ref methods name)))
           (code (pryll:get-internal method)))
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

  (define (pryll:make class . args)
    (dbg "pryll:make " class " " args)
    (apply pryll:new class (list->alist args)))

  (define (fix-class-assoc class hash-name)
    (hash-table-for-each
      (pryll:get-slot class hash-name)
      (lambda (key value)
        (pryll:set-slot! (car value) "associated-class" class))))

  (define (fix-class class)
    (fix-class-assoc class "attributes")
    (fix-class-assoc class "methods"))

  (fix-class <pryll:meta-attribute>)
  (fix-class <pryll:meta-method>)
  (fix-class <pryll:meta-class>)
  (fix-class <pryll:meta-role>)

;;
;;  TESTING
;;

  (define test-class
    (pryll:new <pryll:meta-class>
               (list "attributes"
                     (named->hash
                       (pryll:mkattr "label")))
               (list "methods"
                     (named->hash
                       (pryll:mkmethod
                         "label"
                         (lambda (pos nam)
                           (pryll:get-slot (car pos) "label")))))))

  (define test-object
    (pryll:new
      test-class 
      (list "label" "Foo")))

  (dbg "Label: " (pryll:call-method test-object "label" (list) (mkhash)))

)
