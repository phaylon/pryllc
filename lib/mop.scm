(declare (unit mop))
(declare (uses util))
(declare (uses stack))

(declare (hide object?
               make-object
               object-meta
               object-data
               object-internal))

(import chicken scheme)
(require-extension srfi-69 srfi-1 srfi-13 data-structures lolevel)

(define-record object meta data internal)

(define (pryll:name object)
  (let ((name (pryll:object-data object "name")))
    (if (v-true? name)
      name
      "<unnamed>")))

;(define-record-printer (object instance out)
;  (display
;    (conc "#<instance of "
;          (pryll:name (pryll:meta-for instance))
;          ">"))
;    out)

(define (mop/init object proc)
  (proc (lambda (method . args)
          (pryll:invoke object (->string method) args)))
  object)

(define (mop/describe title item)
;  (say title)
  (define (dumper value)
    (cond ((hash-table? value)
           (map (lambda (key)
                  (list key (dumper (hash-table-ref value key))))
                (hash-table-keys value)))
          (else value)))
  (say
    (list (list "data"
                (dumper (object-data item)))
          (list "internal"
                (dumper (object-internal item))))))

(define (mop/attribute . args)
  (pryll:invoke
    <pryll:meta-attribute>
    "new"
    (list)
    (apply phash args)))

(define (mop/role #!key name)
  (pryll:invoke
    <pryll:meta-role>
    "new"
    (list)
    (phash name: name)))

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

(define (pryll:isa? obj class)
  (pryll:invoke (pryll:meta-for obj) "is-a" (list class)))

(define (pryll:make class . args)
  (pryll:invoke
    class
    "new"
    (list)
    (apply phash args)))

(define mop/meta-string     (make-parameter #f))
(define mop/meta-number     (make-parameter #f))
(define mop/meta-array      (make-parameter #f))
(define mop/meta-hash       (make-parameter #f))
(define mop/meta-void       (make-parameter #f))
(define mop/meta-bool       (make-parameter #f))
(define mop/meta-lambda     (make-parameter #f))
(define mop/meta-integer    (make-parameter #f))

(define (pryll:meta-for item)
  (cond ((object? item)     (force (object-meta item)))
        ((integer? item)    (mop/meta-integer))
        ((number? item)     (mop/meta-number))
        ((string? item)     (mop/meta-string))
        ((list? item)       (mop/meta-array))
        ((hash-table? item) (mop/meta-hash))
        ((void? item)       (mop/meta-void))
        ((boolean? item)    (mop/meta-bool))
        ((procedure? item)  (mop/meta-lambda))
        (else               (error "Unable to find meta for" item))))

(define (pryll:call func pos nam location)
  (cond ((procedure? func)
         (pryll:stack-level
           (pryll:stack-id "lambda"
                           location
                           "")
           (lambda ()
             (func pos nam))))
        (else (pryll:invoke func "call" pos nam))))

(define (make meta #!optional data internal)
  (make-object
    meta
    (or data (mkhash))
    (or internal (mkhash))))

(define (pryll:invoke object method #!optional pos nam fallback loc)
  (dbg "call method " object " " method " " pos " " nam)
  (let* ((meta (pryll:meta-for object))
         (mcache (pryll:object-internal meta "mcache"))
         (proc (phash-slot mcache method)))
    (if (not-void? proc)
      (pryll:stack-level
        (pryll:stack-id "method"
                        loc
                        (conc (pryll:name meta)
                              "."
                              method))
        (lambda ()
          (proc (append (list object) (or pos (list)))
                (or nam (mkhash)))))
      (if fallback
        (fallback)
        (pryll:err <pryll:error-unknown-method>
                   meta:     meta
                   method:   method
                   location: (or loc (void)))))))
;        (error "Invalid method" method)))))

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
(define <pryll:meta-role>)
(define <pryll:meta-method-modifier>)

(define-inline (attr/item name . rest)
  (make (delay <pryll:meta-attribute>)
        (apply phash name: name rest)))

(define-inline (attr/ro name . rest)
  (make (delay <pryll:meta-attribute>)
        (apply
          phash name:       name
                reader:     name
                init-arg:   name
                rest)))

(define-inline (method/predicate method slot)
  (make (delay <pryll:meta-method>)
        (phash name: method
               code: (unwrap-pos-args
                       (lambda (self)
                         (not-void?
                                (pryll:object-data self slot)))))))

(define-inline (method/reader method slot)
  (make (delay <pryll:meta-method>)
        (phash name: method
               code: (unwrap-pos-args
                       (lambda (self)
                         (pryll:object-data self slot))))))

(define-inline (method/hash-getter method slot)
  (make (delay <pryll:meta-method>)
        (phash name: method
               code: (unwrap-pos-args
                      (lambda (self key)
                        (phash-slot (pryll:object-data self slot)
                                    key))))))

(define-inline (method/hash-predicate method slot)
  (make (delay <pryll:meta-method>)
        (phash name: method
               code: (unwrap-pos-args
                       (lambda (self key)
                         (phash-has (pryll:object-data self slot)
                                    key))))))

(define-inline (unique-named ls)
  (delete-duplicates
    ls
    (lambda (a b)
      (string=? (pryll:invoke a "name")
                (pryll:invoke b "name")))))

(define-inline (method/hash-values/super method slot)
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

(define-inline (method/hash-values method slot)
  (make (delay <pryll:meta-method>)
        (phash name: method
               code: (unwrap-pos-args
                      (lambda (self)
                        (phash-values (pryll:object-data
                                       self
                                       slot)))))))

(define-inline (method name code)
  (make (delay <pryll:meta-method>)
        (phash name: name code: code)))

(define-inline (assert-mutable-class class)
  (or (pryll:object-internal class "finalized")
      (error "Finalized class can not be modified")))

(define-inline (method/accessor name slot #!optional preamble)
  (method name
          (unwrap-pos-args
            (lambda (self . rest)
              (and preamble (preamble self))
              (and (not (null? rest))
                   (set! (pryll:object-data self slot) (car rest)))
              (pryll:object-data self slot)))))

(define-inline (all-methods class)
  (if (eqv? class <pryll:meta-class>)
    (phash-values (pryll:object-data class "methods"))
    (pryll:invoke class "get-all-methods")))

(define-inline (calc-icache class)
  (let* ((attrs (pryll:invoke class "get-all-attributes")))
    (set! (pryll:object-internal class "icache")
      (filter (lambda (item)
                (and item (not (void? item))))
              (map (lambda (attr)
                     (pryll:invoke
                       attr
                       "get-cacheable-init-code"))
                   attrs)))))

(define-inline (fix-mcache class)
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

(define-inline (calc-mcache class)
  (let* ((methods (pryll:invoke class "get-all-methods")))
    (set! (pryll:object-internal class "mcache")
      (apply mkhash
             (map (lambda (method)
                    (list (pryll:invoke method "name")
                          (pryll:invoke method "get-cacheable-code")))
                  methods)))))

(set! <pryll:meta-method-modifier>
  (make
    (delay <pryll:meta-class>)
    (phash
      name: "Method::Modifier"
      attributes:
        (phash name:        (attr/ro "name")
               original:    (attr/ro "original")
               code:        (attr/ro "code"))
      methods:
        (phash
          name:
            (method/reader "name" "name")
          code:
            (method/reader "code" "code")
          get-cacheable-code:
            (method
              "get-cacheable-code"
              (unwrap-pos-args
                (lambda (self)
                  (let* ((orig (pryll:invoke
                                 (pryll:object-data self "original")
                                 "get-cacheable-code"))
                         (code (pryll:object-data self "code")))
                    (lambda (pos nam)
                      (code (append (list orig) pos)
                            nam))))))
          call:
            (method
              "call"
              (lambda (pos nam)
                (let* ((code (pryll:invoke self "get-cacheable-code"))
                       (self (car pos)))
                  (code (cdr pos) nam))))
        ))))

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
          code:
            (method/reader "code" "code")
          get-cacheable-code:
            (method "get-cacheable-code"
              (unwrap-pos-args
                (lambda (self)
                  (pryll:object-data self "code"))))
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
      (lambda (self target name)
        (let* ((code (pryll:invoke
                       self
                       (conc "get-" type "-code"))))
          (pryll:invoke
            target
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
                    (unwrap-pos-args
                      (lambda (object new-value)
                        (set! (pryll:object-data object name)
                          new-value)
                        new-value))))))
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
                                (default (list object) (mkhash)))
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
                (lambda (self target)
                  (let* ((reader (pryll:invoke self "reader"))
                         (writer (pryll:invoke self "writer"))
                         (name (pryll:invoke self "name")))
                    (dbg "installing attribute " name)
                    (if (and (v-true? reader)
                             (v-true? writer)
                             (string=? reader writer))
                      (pryll:invoke
                        self
                        "install-accessor"
                        (liast target reader))
                      (begin
                        (if (v-true? reader)
                          (pryll:invoke self
                                        "install-reader"
                                        (list target reader)))
                        (if (v-true? writer)
                          (pryll:invoke self
                                        "install-writer"
                                        (list target writer)))))
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
                                (set! (pryll:object-data object name)
                                  (default (list object) (mkhash))))
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
      method)))

(define (class-add-method-modifier pos nam)
  (let* ((self (car pos))
         (name (cadr pos))
         (code (caddr pos))
         (curr (pryll:invoke self "find-method" (list name))))
    (if (not-void? curr)
      (set! (phash-slot (pryll:object-data self "methods") name)
        (pryll:invoke
          <pryll:meta-method-modifier>
          "new"
          (list)
          (phash original:  curr
                 name:      name
                 code:      code)))
      (error "Cannot wrap unknown method" name))
    (void)))

(define (class-add-attribute pos nam)
  (let* ((self (car pos))
         (attr (cadr pos)))
    ; TODO check for existing attrs
    (set! (phash-slot (pryll:object-data self "attributes")
                      (pryll:invoke attr "name"))
      attr)
      (pryll:invoke attr "install" (list self))))

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
          copy:
            (method
              "copy"
              (unwrap-pos-args
                (lambda (self instance)
                  (make
                    (object-meta instance)
                    (hash-table-copy
                      (object-data instance))
                    (hash-table-copy
                      (object-internal instance))))))
          is-a:
            ;; TODO cache inheritance in objects
            (method
              "is-a"
              (unwrap-pos-args
                (lambda (self class)
                  (if (equal? (object->pointer self)
                              (object->pointer class))
                    #t
                    (let ((super (pryll:invoke self "superclass")))
                      (if (void? super)
                        #f
                        (pryll:invoke super "is-a" (list class))))))))
          finalize:
            (method
              "finalize"
              (unwrap-pos-args
                (lambda (self)
                  (dbg "finalize " self)
                  (calc-mcache self)
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
          add-method-modifier:
            (method "add-method-modifier" class-add-method-modifier)
          add-method:
            (method "add-method" class-add-method)
          add-attribute:
            (method "add-attribute" class-add-attribute)
          add-roles:
            (method
              "add-roles"
              (lambda (pos nam)
                (map (lambda (item)
                       (pryll:invoke (car pos)
                                     "add-role"
                                     (list item)))
                     (cdr pos))))
          add-role:
            (method
              "add-role"
              (lambda (pos nam)
                (pryll:invoke
                  (cadr pos)
                  "apply"
                  (list (car pos)))))
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
          find-method:
            (method
              "find-method"
              (unwrap-pos-args
                (lambda (self name)
                  (if (pryll:invoke self "has-method" (list name))
                    (pryll:invoke self "get-method" (list name))
                    (let ((super (pryll:invoke self "superclass")))
                      (if (not-void? super)
                        (pryll:invoke super "find-method" (list name))
                        (void)))))))
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

(for-each fix-mcache
          (list <pryll:meta-class>
                <pryll:meta-attribute>
                <pryll:meta-method>
                <pryll:meta-method-modifier>))

(for-each calc-icache
          (list <pryll:meta-class>
                <pryll:meta-attribute>
                <pryll:meta-method>
                <pryll:meta-method-modifier>))

(define (mop/copy object)
  (pryll:invoke
    (pryll:meta-for object)
    "copy"
    (list object)))

(define <pryll:meta-role-apply-attribute>
  (mop/init
    (mop/class name: "Role::Apply::Attribute")
    (lambda (call)
      (call add-attributes:
            (attr/ro "attribute"))
      (call add-methods:
            (method
              "apply"
              (unwrap-pos-args
                (lambda (self target)
                  (pryll:invoke
                    target
                    "add-attribute"
                    (list (pryll:invoke self "attribute")))))))
      (call finalize:))))

(define <pryll:meta-role-apply-method>
  (mop/init
    (mop/class name: "Role::Apply::Method")
    (lambda (call)
      (call add-attributes:
            (attr/ro "method"))
      (call add-methods:
            (method
              "apply"
              (unwrap-pos-args
                (lambda (self target)
                  (pryll:invoke
                    target
                    "add-method"
                    (list (pryll:invoke self "method")))))))
      (call finalize:))))

(define <pryll:meta-role-apply-method-modifier>
  (mop/init
    (mop/class name: "Role::Apply::Method::Modifier")
    (lambda (call)
      (call add-attributes:
            (attr/ro "name")
            (attr/ro "code"))
      (call add-methods:
            (method
              "apply"
              (unwrap-pos-args
                (lambda (self target)
                  (pryll:invoke
                    target
                    "add-method-modifier"
                    (list (pryll:object-data self "name")
                          (pryll:object-data self "code")))))))
      (call finalize:))))

(set! <pryll:meta-role>
  (mop/init
    (mop/class name: "Role")
    (lambda (call)
      (call add-attributes:
            (attr/ro "name")
            (attr/ro "sequence"
                     default: (lambda args (list))))
      (call add-methods:
            (method
              "extend-sequence"
              (lambda (pos nam)
                (let ((self (car pos)))
                  (set! (pryll:object-data self "sequence")
                    (append
                      (pryll:object-data self "sequence")
                      (cdr pos))))
                (void)))
            (method
              "add-method-modifier"
              (unwrap-pos-args
                (lambda (self name code)
                  (pryll:invoke
                    self
                    "extend-sequence"
                    (list
                      (pryll:make
                        <pryll:meta-role-apply-method-modifier>
                        name: name
                        code: code))))))
            (method
              "add-method"
              (unwrap-pos-args
                (lambda (self method)
                  (pryll:invoke
                    self
                    "extend-sequence"
                    (list
                      (pryll:make
                        <pryll:meta-role-apply-method>
                        method: method)))
                  (void))))
            (method
              "add-attribute"
              (unwrap-pos-args
                (lambda (self attr)
                  (pryll:invoke
                    self
                    "extend-sequence"
                    (list
                      (pryll:make
                        <pryll:meta-role-apply-attribute>
                        attribute: attr)))
                  (void))))
            (method
              "apply"
              (unwrap-pos-args
                (lambda (self target)
                  (map (lambda (app)
                         (pryll:invoke app "apply" (list target)))
                       (pryll:object-data self "sequence"))))))
      (call finalize:))))

(define (mop/method #!key name code)
  (method name code))

