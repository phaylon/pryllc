(declare (unit ast/signatures))
(declare (uses ast/util mop compiler errors))

(import chicken scheme)

(define <pryll:ast-signature-param-pos>
  (mop/init
    (mop/class name: "Core::AST::Signature::Parameter::Positional")
    (lambda (call)
      (call add-attributes:
            (attr/item "type")
            (attr/item "variable")
            (attr/item "is-optional")
            (attr/item "default"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(positional
                   ,(dump-slot self "variable")
                   ,(let ((type (pryll:object-data self "type")))
                      (if type
                        `(type ,(dump type))
                        'no-type))
                   ,(let ((default (pryll:object-data self "default")))
                      (if default
                        `(default ,(dump default))
                        'no-default))
                   ,(let ((opt (pryll:object-data self "is-optional")))
                      (if opt 'is-optional 'is-required))))))
      (call finalize:))))

(define (make-signature-param-pos type var optional default)
  (pryll:make <pryll:ast-signature-param-pos>
              type:         type
              variable:     var
              is-optional:  (or default optional)
              default:      default))

(define <pryll:ast-signature-param-nam>
  (mop/init
    (mop/class name: "Core::AST::Signature::Parameter::Named")
    (lambda (call)
      (call add-attributes:
            (attr/item "type")
            (attr/item "variable")
            (attr/item "is-optional")
            (attr/item "default"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(named
                   ,(dump-slot self "variable")
                   ,(let ((type (pryll:object-data self "type")))
                      (if type
                        `(type ,(dump type))
                        'no-type))
                   ,(let ((default (pryll:object-data self "default")))
                      (if default
                        `(default ,(dump default))
                        'no-default))
                   ,(let ((opt (pryll:object-data self "is-optional")))
                      (if opt 'is-optional 'is-required))))))
      (call finalize:))))

(define (make-signature-param-nam type var optional default)
  (pryll:make <pryll:ast-signature-param-nam>
              type:         type
              variable:     var
              is-optional:  (or default optional)
              default:      default))

(define <pryll:ast-signature-rest-pos>
  (mop/init
    (mop/class name: "Core::AST::Signature::Rest::Positional")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "variable"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(positional-rest
                   ,(dump-slot self "variable")))))
      (call finalize:))))

(define (make-signature-rest-pos token variable)
  (pryll:make <pryll:ast-signature-rest-pos>
              location: (token-location token)
              variable: variable))

(define <pryll:ast-signature-rest-nam>
  (mop/init
    (mop/class name: "Core::AST::Signature::Rest::Named")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "variable"))
      (call add-methods:
            (dump-method
              (lambda (self)
                `(named-rest
                   ,(dump-slot self "variable")))))
      (call finalize:))))

(define (make-signature-rest-nam token variable)
  (pryll:make <pryll:ast-signature-rest-nam>
              location: (token-location token)
              variable: variable))

(define-inline (positional-rest? value)
  (pryll:isa? value <pryll:ast-signature-rest-pos>))

(define-inline (named-rest? value)
  (pryll:isa? value <pryll:ast-signature-rest-nam>))

(define-inline (named-param? value)
  (pryll:isa? value <pryll:ast-signature-param-nam>))

(define-inline (positional-param? value)
  (pryll:isa? value <pryll:ast-signature-param-pos>))

(define-inline (max-positionals self)
  (call/cc
    (lambda (cont)
      (length
        (filter (lambda (item)
                  (if (positional-param? item)
                    #t
                    (if (positional-rest? item)
                      (cont #f)
                      #f)))
                (pryll:object-data self "parameters"))))))

(define-inline (min-positionals self)
  (length
    (filter (lambda (item)
              (if (positional-param? item)
                (if (pryll:invoke item "is-optional")
                  #f
                  #t)
                #f))
            (pryll:object-data self "parameters"))))

(define-inline (named-required self)
  (map (lambda (item)
         (pryll:invoke
           (pryll:invoke item "variable")
           "identifier"))
       (filter (lambda (item)
                 (and (named-param? item)
                      (not (pryll:invoke item "is-optional"))))
               (pryll:object-data self "parameters"))))

(define-inline (signature-declare-pos self ctx src-var)
  (define (compile-pos left index)
    (if (null? left)
      '()
      (let* ((param (car left))
             (default (pryll:invoke param "default"))
             (var (compile/scoped-var
                    (pryll:invoke
                      (pryll:invoke param "variable")
                      "value")))
             (spec (list
                     (pryll:invoke var "symbol")
                     `(if (> ,index (- (vector-length ,src-var) 1))
                        ,(if default
                           (compile ctx default)
                           '(void))
                        (list-ref ,src-var ,index)))))
        (pryll:invoke ctx "add-variable" (vector var))
        (cons spec
              (compile-pos (cdr left) (+ index 1))))))
  (let* ((params (pryll:object-data self "parameters"))
         (pos (filter positional-param? params)))
    ;; TODO sanity checks
    (append
      (compile-pos pos 0))))

(define-inline (signature-declare-nam self ctx src-var)
  (let* ((params (pryll:object-data self "parameters"))
         (nam (filter named-param? params)))
    (map (lambda (param)
           (let* ((default (pryll:invoke param "default"))
                  (name (pryll:invoke
                          (pryll:invoke param "variable")
                          "identifier"))
                  (var (compile/scoped-var
                         (pryll:invoke
                           (pryll:invoke param "variable")
                           "value")))
                  (spec (list
                          (pryll:invoke var "symbol")
                          `(if (hash-table-exists? ,src-var ,name)
                             (hash-table-ref ,src-var ,name)
                             ,(if default
                                (compile ctx default)
                                '(void))))))
             (pryll:invoke ctx "add-variable" (vector var))
             spec))
         nam)))

(define-inline (signature-declare-rest-pos self ctx src-var)
  (let* ((params (pryll:object-data self "parameters"))
         (pos-cnt (length (filter positional-param? params)))
         (rest (filter positional-rest? params)))
    (if (= 0 (length rest))
      (list)
      (let* ((rest-param (car rest))
             (lexvar (pryll:invoke rest-param "variable"))
             (var (compile/scoped-var
                    (pryll:invoke lexvar "value"))))
        (pryll:invoke ctx "add-variable" (vector var))
        (list
          (list (pryll:invoke var "symbol")
                `(if (> (vector-length ,src-var) ,pos-cnt)
                   (vtail ,src-var ,pos-cnt)
                   (vector))))))))

(define-inline (signature-declare-rest-nam self ctx src-var)
  (let* ((params (pryll:object-data self "parameters"))
         (known (map (lambda (param)
                       (pryll:invoke
                         (pryll:invoke param "variable")
                         "identifier"))
                     (filter named-param? params)))
         (rest (filter named-rest? params)))
    (if (= 0 (length rest))
      (list)
      (let* ((rest-param (car rest))
             (lexvar (pryll:invoke rest-param "variable"))
             (var-pair (compile/genvar 'pair))
             (var (compile/scoped-var
                    (pryll:invoke lexvar "value"))))
        (pryll:invoke ctx "add-variable" (vector var))
        (list
          (list (pryll:invoke var "symbol")
                `(alist->hash-table
                   (filter (lambda (,var-pair)
                             (not (contains-str
                                    (list ,@known)
                                    (car ,var-pair))))
                           (hash-table->alist ,src-var)))))))))

(define-inline (all-named self)
  (map (lambda (param)
         (pryll:invoke (pryll:invoke param "variable") "identifier"))
       (filter named-param? (pryll:invoke self "parameters"))))

(define-inline (has-named-rest self)
  (< 0 (length (filter named-rest? (pryll:invoke self "parameters")))))

(define (compile-signature-scope self ctx var-pos var-nam block)
  (let ((var-key (compile/genvar 'key))
        (var-unknown (compile/genvar 'missing))
        (pos-min (min-positionals self))
        (pos-max (max-positionals self))
        (nam-req (named-required self))
        (nam-known (all-named self))
        (nam-rest (has-named-rest self))
        (subctx  (subcontext ctx)))
    `(begin
       ,@(if (and pos-min (> pos-min 0))
           `((if (< (vector-length ,var-pos) ,pos-min)
               (pryll:err
                 <pryll:error-arguments>
                 message:
                 (conc
                   ,(conc
                      "Expected at least "
                      pos-min
                      " positional "
                      (if (= pos-min 1) "argument" "arguments")
                      ", but only received ")
                   (vector-length ,var-pos)))))
           '())
       ,@(if pos-max
           `((if (> (vector-length ,var-pos) ,pos-max)
               (pryll:err
                 <pryll:error-arguments>
                 message:
                 (conc
                   ,(conc
                      "Expected at most "
                      pos-max
                      " positional "
                      (if (= pos-max 1) "argument" "arguments")
                      ", but received ")
                   (vector-length ,var-pos)))))
           '())
       ,@(if (> (length nam-req) 0)
           `((for-each (lambda (,var-key)
                         (if (not (hash-table-exists? ,var-nam ,var-key))
                           (error "Missing named argument" ,var-key)))
                       (list ,@nam-req)))
           '())
       ,@(if nam-rest
           '()
           `((let ((,var-unknown (str-unknown
                                   (list ,@nam-known)
                                   (hash-table-keys ,var-nam))))
               (if (< 0 (length ,var-unknown))
                 (error "Unknown named args" ,var-unknown)))))
       (let ,(append
               (signature-declare-pos self subctx var-pos)
               (signature-declare-nam self subctx var-nam)
               (signature-declare-rest-pos self subctx var-pos)
               (signature-declare-rest-nam self subctx var-nam))
         ,(compile subctx block)))))

(define <pryll:ast-signature>
  (mop/init
    (mop/class name: "Core::AST::Signature")
    (lambda (call)
      (call add-attributes:
            (attr/item "parameters"))
      (call add-methods:
            (mop/method
              name: "compile-scope"
              code: (lambda (pos nam)
                      (apply compile-signature-scope
                             (vector->list pos))))
            (dump-method
              (lambda (self)
                `(signature
                   ,@(map dump (pryll:object-data self "parameters"))))))
      (call finalize:))))

(define (make-signature parameters)
  (pryll:make <pryll:ast-signature>
              parameters: parameters))

