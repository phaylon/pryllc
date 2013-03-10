(declare (uses util))
(declare (unit mop))

(module pryll/mop
  (make
   mop/class
   mop/attribute
   mop/method
   mop/init
   pryll:invoke
   pryll:make
   pryll:object-data
   <pryll:meta-class>
   <pryll:meta-attribute>
   <pryll:meta-method>)

  (import pryll/util)
  (import chicken scheme)
  (require-extension srfi-69 srfi-1 srfi-13 data-structures)

  (define-record object meta data internal)

  (define (mop/init object proc)
    (proc (lambda (method . args)
            (pryll:invoke object (->string method) args)))
    object)

  (define (mop/attribute . args)
    (pryll:invoke
      <pryll:meta-attribute>
      "new"
      (list)
      (apply phash args)))

  (define (mop/method #!key name code)
    (method name code))

  (define (mop/class #!key name extends)
    (pryll:invoke
      <pryll:meta-class>
      "new"
      (list)
      (apply phash
             (append
               (list name: name)
               (if (v-true? extends)
                 (list superclass: extends)
                 (list))))))

  (define (pryll:make class . args)
    (pryll:invoke
      class
      "new"
      (list)
      (apply phash args)))

  (define (pryll:meta-for item)
    (cond ((object? item)
           (force (object-meta item)))
          (else
           (error "Unable to find meta for" item))))

  (define (make meta #!optional data internal)
    (make-object
      meta
      (or data (mkhash))
      (or internal (mkhash))))

  (define (pryll:invoke object method #!optional pos nam)
    (dbg "call method " object " " method " " pos " " nam)
    (let* ((meta (pryll:meta-for object))
           (mcache (pryll:object-internal meta "mcache"))
           (proc (phash-slot mcache method)))
;      (dbg "proc " proc)
      (if (not-void? proc)
        (proc (append (list object) (or pos (list)))
              (or nam (mkhash)))
        (error "Invalid method" method))))

  (define pryll:object-data
    (getter-with-setter
      (lambda (object key . rest)
        (apply phash-slot (object-data object) (->string key) rest))
      (lambda (object key new-value)
        (set! (phash-slot (object-data object) (->string key))
          new-value))))

  (define (pryll:object-has object key)
    (phash-has (object-data object) key))

  (define pryll:object-internal
    (getter-with-setter
      (lambda (object key)
        (phash-slot (object-internal object) (->string key)))
      (lambda (object key new-value)
        (set! (phash-slot (object-internal object) (->string key))
          new-value))))

  (define (unwrap-pos-args code)
    (lambda (pos nam)
      (apply code pos)))

  (define <pryll:meta-class>)
  (define <pryll:meta-attribute>)
  (define <pryll:meta-method>)

  (define (attr/item name . rest)
    (make (delay <pryll:meta-attribute>)
          (apply phash name: name rest)))

  (define (attr/ro name . rest)
    (make (delay <pryll:meta-attribute>)
          (apply
            phash name:       name
                  reader:     name
                  init-arg:   name
                  rest)))

  (define (method/predicate method slot)
    (make (delay <pryll:meta-method>)
          (phash name: method
                 code: (unwrap-pos-args
                         (lambda (self)
                           (not-void?
                                  (pryll:object-data self slot)))))))

  (define (method/reader method slot)
    (make (delay <pryll:meta-method>)
          (phash name: method
                 code: (unwrap-pos-args
                         (lambda (self)
                           (pryll:object-data self slot))))))

  (define (method/hash-getter method slot)
    (make (delay <pryll:meta-method>)
          (phash name: method
                 code: (unwrap-pos-args
                        (lambda (self key)
                          (phash-slot (pryll:object-data self slot)
                                      key))))))

  (define (method/hash-predicate method slot)
    (make (delay <pryll:meta-method>)
          (phash name: method
                 code: (unwrap-pos-args
                         (lambda (self key)
                           (phash-has (pryll:object-data self slot)
                                      key))))))

  (define (unique-named ls)
    (delete-duplicates
      ls
      (lambda (a b)
        (string=? (pryll:invoke a "name")
                  (pryll:invoke b "name")))))

  (define (method/hash-values/super method slot)
    (make (delay <pryll:meta-method>)
          (phash name: method
                 code: (unwrap-pos-args
                         (lambda (self)
                           (let ((data (pryll:object-data
                                         self
                                         slot
                                         (lambda () (mkhash))))
                                 (super (pryll:object-data
                                          self
                                          "superclass")))
                             (unique-named
                                (append (phash-values data)
                                        (if (not-void? super)
                                          (pryll:invoke
                                            super
                                            method)
                                          '())))))))))

  (define (method/hash-values method slot)
    (make (delay <pryll:meta-method>)
          (phash name: method
                 code: (unwrap-pos-args
                        (lambda (self)
                          (phash-values (pryll:object-data
                                         self
                                         slot)))))))

  (define (method/associate-with #!optional then)
    (define slot "associated-class")
    (method "associate-with"
            (unwrap-pos-args
              (lambda (self class)
                (and (not-void? (pryll:object-data self slot))
                     (error "Meta object already associated"))
                (set! (pryll:object-data self slot)
                  class)
                (dbg "associated " self " with " class)
                (and then (then self))))))

  (define (method name code)
    (make (delay <pryll:meta-method>)
          (phash name: name code: code)))

  (define (assert-mutable-class class)
    (or (pryll:object-internal class "finalized")
        (error "Finalized class can not be modified")))

  (define (method/accessor name slot #!optional preamble)
    (method name
            (unwrap-pos-args
              (lambda (self . rest)
                (and preamble (preamble self))
                (and (not (null? rest))
                     (set! (pryll:object-data self slot) (car rest)))
                (pryll:object-data self slot)))))

  (define (all-methods class)
    (if (eqv? class <pryll:meta-class>)
      (phash-values (pryll:object-data class "methods"))
      (pryll:invoke class "get-all-methods")))

  (define (calc-icache class)
    (let* ((attrs (pryll:invoke class "get-all-attributes")))
      (set! (pryll:object-internal class "icache")
        (filter (lambda (item)
                  (and item (not (void? item))))
                (map (lambda (attr)
                       (pryll:invoke
                         attr
                         "get-cacheable-init-code"))
                     attrs)))))

  (define (fix-mcache class)
    (let* ((methods (phash-values (pryll:object-data
                                    class
                                    "methods"
                                    (lambda () (mkhash))))))
      (set! (pryll:object-internal class "mcache")
        (apply mkhash
               (map (lambda (method)
                      (list (pryll:object-data method "name")
                            (pryll:object-data method "code")))
                    methods)))))

  (define (calc-mcache class)
    (let* ((methods (all-methods class)))
      (set! (pryll:object-internal class "mcache")
        (apply mkhash
               (map (lambda (method)
                      (pryll:invoke method "get-cacheable-code"))
                    methods)))))

  (set! <pryll:meta-method>
    (make
      (delay <pryll:meta-class>)
      (phash
        name: "Method"
        attributes:
          (phash name: (attr/ro "name")
                 code: (attr/ro "code"))
        methods:
          (phash
            name:
              (method/reader "name" "name")
            get-cacheable-code:
              (method "get-cacheable-code"
                (unwrap-pos-args
                  (lambda (self)
                    (pryll:object-data self "code"))))
            associate-with:
              (method/associate-with)
            call:
              (method "call"
                (lambda (pos nam)
                  (let* ((self (car pos))
                         (realpos (cdr pos))
                         (code (pryll:object-data
                                 self
                                 "code")))
                    (code realpos nam))))
            ))))        

  (define (method/installer type)
    (method
      (conc "install-" type)
      (unwrap-pos-args
        (lambda (self name)
          (let* ((code (pryll:invoke
                         self
                         (conc "get-" type "-code")))
                 (class (pryll:invoke self "associated-class")))
            (pryll:invoke
              class
              "add-method"
              (list (method name code))))))))

  (set! <pryll:meta-attribute>
    (make
      (delay <pryll:meta-class>)
      (phash
        name: "Attribute"
        attributes:
          (phash name:              (attr/ro "name")
                 reader:            (attr/ro "reader")
                 writer:            (attr/ro "writer")
                 is-required:       (attr/ro "is-required")
                 is-lazy:           (attr/ro "is-lazy")
                 default:           (attr/ro "default")
                 associated-class:  (attr/ro "associated-class")
                 init-arg:          (attr/ro "init-arg"))
        methods:
          (phash
            name:
              (method/reader "name" "name")
            writer:
              (method/reader "writer" "writer")
            reader:
              (method/reader "reader" "reader")
            is-required:
              (method/reader "is-required" "is-required")
            init-arg:
              (method/reader "init-arg" "init-arg")
            is-lazy:
              (method/reader "is-lazy" "is-lazy")
            default:
              (method/reader "default" "default")
            associated-class:
              (method/reader "associated-class" "associated-class")
            associate-with:
              (method/associate-with
                (lambda (self)
                  (pryll:invoke self "install")))
            get-accessor-code:
              (method
                "get-accessor-code"
                (unwrap-pos-args
                  (lambda (self)
                    (let* ((get (pryll:invoke self "get-reader-code"))
                           (set (pryll:invoke self "get-writer-code")))
                      (lambda (pos nam)
                        (let* ((object (car pos))
                               (args (cdr pos)))
                          (if (null? args)
                            (get object)
                            (set object (car args)))))))))
            get-writer-code:
              ; TODO triggers, type constraints
              (method
                "get-writer-code"
                (unwrap-pos-args
                  (lambda (self)
                    (let* ((name (pryll:invoke self "name")))
                      (lambda (object new-value)
                        (set! (pryll:object-data object name)
                          new-value)
                        new-value)))))
            get-reader-code:
              (method
                "get-reader-code"
                (unwrap-pos-args
                  (lambda (self)
                    (let* ((name (pryll:invoke self "name"))
                           (lazy (pryll:invoke self "is-lazy"))
                           (default (pryll:invoke self "default")))
                      ; TODO die if lazy without default
                      (unwrap-pos-args
                        (if lazy
                          (lambda (object)
                            (if (pryll:object-has object name)
                              (pryll:object-data object name)
                              (begin
                                (set! (pryll:object-data object name)
                                  (default object))
                                (pryll:object-data object name))))
                          (lambda (object)
                            (pryll:object-data object name))))))))
            install-accessor:
              (method/installer "accessor")
            install-writer:
              (method/installer "writer")
            install-reader:
              (method/installer "reader")
            install:
              (method
                "install"
                (unwrap-pos-args
                  (lambda (self)
                    (let* ((reader (pryll:invoke self "reader"))
                           (writer (pryll:invoke self "writer"))
                           (name (pryll:invoke self "name"))
                           (class (pryll:invoke self "associated-class")))
                      (dbg "installing attribute " name)
                      (if (and (v-true? reader)
                               (v-true? writer)
                               (string=? reader writer))
                        (pryll:invoke self "install-accessor" reader)
                        (begin
                          (if (v-true? reader)
                            (pryll:invoke self
                                          "install-reader"
                                          (list reader)))
                          (if (v-true? writer)
                            (pryll:invoke self
                                          "install-writer"
                                          (list writer)))))
                      (void)))))
            get-cacheable-init-code:
              (method
                "get-cacheable-init-code"
                (unwrap-pos-args
                  (lambda (self)
                    (let* ((init-arg (pryll:invoke self "init-arg"))
                           (name (pryll:invoke self "name"))
                           (default (pryll:invoke self "default"))
                           (lazy (pryll:invoke self "is-lazy"))
                           (required (if (not-void? default)
                                       #f
                                       (v-true?
                                         (pryll:invoke
                                           self
                                           "is-required")))))
                      (if (or init-arg (and default (not lazy)))
                        (unwrap-pos-args
                          (lambda (object args)
                            (dbg "attribute init " name " " default)
                            (if (phash-has args init-arg)
                              (set! (pryll:object-data object name)
                                (phash-slot args init-arg))
                              (if (v-true? default)
                                (if (v-false? lazy)
                                  (begin
                                    (dbg "default")
                                  (set! (pryll:object-data object name)
                                    (default object)))
                                  )
                                (if required
                                  (error "Required attribute" name))))
                            (void)))
                        #f)))))
            get:
              (method
                "get"
                (unwrap-pos-args
                  (lambda (self object)
                    (pryll:object-data
                      object
                      (pryll:object-data
                        self
                        "name")))))
            set:
              (method
                "set"
                (unwrap-pos-args
                  (lambda (self object value)
                    (set! (pryll:object-data
                            object
                            (pryll:object-data
                              self
                              "name"))
                      value))))
              ))))

  (define (construct-instance class pos nam)
    (let* ((object (make class)))
      (map (lambda (init)
             (init (list object nam) (mkhash)))
           (pryll:object-internal class "icache"))
      object))

  (define (class-add-method pos nam)
    (let* ((self (car pos))
           (method (cadr pos)))
      ; TODO check for existing methods
      (set! (phash-slot (pryll:object-data self "methods")
                        (pryll:invoke method "name"))
        method)
      (pryll:invoke method "associate-with" (list self))
      ))

  (define (class-add-attribute pos nam)
    (let* ((self (car pos))
           (attr (cadr pos)))
      ; TODO check for existing attrs
      (set! (phash-slot (pryll:object-data self "attributes")
                        (pryll:invoke attr "name"))
        attr)
      (pryll:invoke attr "associate-with" (list self))))

  (set! <pryll:meta-class>
    (make
      (delay <pryll:meta-class>)
      (phash
        name: "Class"
        attributes:
          (phash name:       (attr/ro "name")
                 attributes: (attr/item "attributes"
                                        default: (lambda args (mkhash)))
                 methods:    (attr/item "methods"
                                        default: (lambda args (mkhash)))
                 superclass: (attr/ro "superclass"))
        methods:
          (phash
            new:
              (method
                "new"
                (lambda (pos nam)
                  (construct-instance (car pos) (cdr pos) nam)))
            finalize:
              (method
                "finalize"
                (unwrap-pos-args
                  (lambda (self)
                    (dbg "finalize " self)
                    (fix-mcache self)
                    (dbg "mcache fixed")
                    (calc-icache self)
                    (dbg "icache calculated")
                    #t)))
            name:
              (method/accessor "name" "name" assert-mutable-class)
            superclass:
              (method/accessor
                "superclass"
                "superclass"
                assert-mutable-class)
            add-method:
              (method "add-method" class-add-method)
            add-attribute:
              (method "add-attribute" class-add-attribute)
            add-methods:
              (method
                "add-methods"
                (lambda (pos nam)
                  (map (lambda (item)
                         (pryll:invoke (car pos)
                                       "add-method"
                                       (list item)))
                       (cdr pos))))
            add-attributes:
              (method
                "add-attributes"
                (lambda (pos nam)
                  (map (lambda (item)
                         (pryll:invoke (car pos)
                                       "add-attribute"
                                       (list item)))
                       (cdr pos))))
            has-attribute:
              (method/hash-predicate "has-attribute" "attributes")
            get-attribute:
              (method/hash-getter "get-attribute" "attributes")
            has-method:
              (method/hash-predicate "has-method" "methods")
            get-method:
              (method/hash-getter "get-method" "methods")
            get-methods:
              (method/hash-values "get-methods" "methods")
            get-all-attributes:
              (method/hash-values/super "get-all-attributes" "attributes")
            get-all-methods:
              (method/hash-values/super "get-all-methods" "methods")
            ))))

  (define (fix-assoc slot class)
    (for-each (lambda (item)
                (dbg "associate " item " with " class)
                (pryll:invoke item "associate-with" (list class)))
              (phash-values
                (pryll:object-data class slot))))

  (for-each fix-mcache
            (list <pryll:meta-class>
                  <pryll:meta-attribute>
                  <pryll:meta-method>))

  (for-each calc-icache
            (list <pryll:meta-class>
                  <pryll:meta-attribute>
                  <pryll:meta-method>))

  (for-each (lambda (class)
              (dbg "fixing " class)
              (fix-assoc "methods" class))
            (list
              <pryll:meta-class>
              <pryll:meta-attribute>
              <pryll:meta-method>))

)
