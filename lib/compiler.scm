(declare (unit compiler))
(declare
  (uses mop
        util
        exceptions
        errors
        meta/number))

(import chicken scheme)
(require-extension srfi-1 srfi-13 srfi-69)

(define (compile/genvar #!optional name)
  (gensym (conc "pry/lex-" (->string name) "-")))

(define-inline (is-call? symbols value)
  (and (list? value)
       (not (null? value))
       (let ((first-item (car value)))
         (length (filter (lambda (sym)
                           (equal? sym first-item))
                         symbols)))))

(define type/string
  (list '<pryll:meta-string>
        'string?
        (lambda (value)
          (or (string? value)
              (is-call? '(conc sprintf) value)))))

(define type/number
  (list '<pryll:meta-number>
        'number?
        (lambda (value)
          (or (number? value)
              (is-call? '(+ - * /) value)))))

(define type/array
  (list '<pryll:meta-array>
        'list?
        (lambda (value)
          (is-call? '(list append map filter) value))))

(define type/hash
  (list '<pryll:meta-hash>
        'hash-table?
        (lambda (value)
          (is-call? '(alist->hash-table) value))))

(define-inline (type-meta type) (car type))
(define-inline (type-pred type) (cadr type))
(define-inline (type-const type) (caddr type))

(define (compile/compile-typechecked ctx type ast . rest)
  (compile/assert-type
    type
    (if (length rest)
      (car rest)
      (pryll:invoke ast "compile" (list ctx)))
    (pryll:invoke ast "location")))

(define (compile/assert-type type expression #!optional location)
  (if ((type-const type) expression)
    expression
    (let* ((var-value (compile/genvar 'value)))
      `(let ((,var-value ,expression))
         (if (,(type-pred type) ,var-value)
           ,var-value
           (pryll:throw
             (pryll:make
               <pryll:error-type>
               ,@(if location
                   `(location: (list ,@location))
                   '())
               expected: ,(type-meta type)
               received: (pryll:meta-for ,var-value))))))))

(define (compile-with-return ctx proc)
  (let ((var-ret (compile/genvar 'ret))
        (rctx (subcontext ctx)))
    (pryll:invoke rctx "set-return" (list var-ret))
    `(call/cc
       (lambda (,var-ret)
         ,(proc rctx)))))

(define <compiler-var/scope>
  (mop/init
    (mop/class name: "Core::AST::Compiler::Variable::Scoped")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:         "name"
              reader:       "name"
              init-arg:     "name"
              is-required:  #t)
            (mop/attribute
              name:         "location"
              reader:       "location"
              init-arg:     "location")
            (mop/attribute
              name:         "symbol"
              reader:       "symbol"
              default:      (lambda (pos nam)
                              (compile/genvar
                                (pryll:object-data (car pos) "name")))
              is-lazy:      #t))
      (call add-methods:
            (mop/method
              name: "compile-access"
              code: (lambda (pos nam)
                      (let* ((self (car pos))
                             (var (pryll:invoke self "symbol")))
                        var)))
            (mop/method
              name: "compile-declare"
              code: (lambda (pos nam)
                      (let* ((self (car pos))
                             (ctx (cadr pos))
                             (init (caddr pos))
                             (var (pryll:invoke self "symbol")))
                        `(define
                           ,var
                           ,(if init
                              (pryll:invoke init "compile" (list ctx))
                              `(void)))))))
      (call finalize:))))

(define (compile/scoped-var name #!optional loc)
  (pryll:make <compiler-var/scope>
              name:     name
              location: (or loc (void))))

(define-inline (declaration? item)
  (or (pryll:isa? item <pryll:ast-subroutine>)
      (pryll:isa? item <pryll:ast-function>)
      (pryll:isa? item <pryll:ast-module>)))

(define (compile/statements ctx seq)
  `(begin
     ,@(filter (lambda (item) item)
               (map (lambda (item)
                      (if (declaration? item)
                        (pryll:invoke ctx "predeclare" (list item))
                        #f))
                    seq))
     ,@(map (lambda (item)
              (let ((code (pryll:invoke item "compile" (list ctx))))
                (pryll:invoke ctx "commit-variables")
                code))
            seq)))

(define (ensure-new-var self var)
  (let* ((name (pryll:invoke var "name"))
         (loc (pryll:invoke var "location"))
         (known (or (hash-table-exists?
                      (pryll:object-data self "variables")
                      name)
                    (hash-table-exists?
                      (pryll:object-data self "prepared")
                      name))))
    (if known
      (pryll:err <pryll:error-syntax>
                 location: loc
                 message:  (conc "Illegal redeclaration of variable "
                                 name)))))

(define-inline (callable-type ast)
  (cond ((pryll:isa? ast <pryll:ast-subroutine>)
         "subroutine")
        ((pryll:isa? ast <pryll:ast-function>)
         "function")
        (else (error "Unexpected callable type"))))

(define-inline (to-name value)
;  (dbg "to-name " (pryll:name value))
  (cond ((string? value)
         value)
        ((pryll:isa? value <pryll:ast-namespace>)
         (pryll:invoke value "name"))
        (else (error "Unable to convert to name" value))))

(define (compile/with-special-var ctx vname source proc)
  (let* ((lex (compile/genvar vname))
         (subctx (subcontext/customized
                   ctx
                   special-variables:
                   (alist->hash-table
                     (list (cons vname
                                 (lambda () lex)))))))
    `(let ((,lex ,source))
       ,(proc subctx lex))))

(define (ensure-new-identifier self ast)
  (let* ((name (to-name (pryll:invoke ast "name")))
         (loc (pryll:invoke ast "location"))
         (known (or (hash-table-exists?
                      (pryll:object-data self "identifiers")
                      name))))
    (if known
      (pryll:err <pryll:error-syntax>
                 location: loc
                 message:  (conc "Illegal redeclaration of "
                                 (callable-type ast)
                                 " '"
                                 name
                                 "'")))))

(define-inline (call-parent self method . args)
  (let ((parent (pryll:object-data self "parent")))
    (if (not-void? parent)
      (pryll:invoke parent method args)
      #f)))

(define <ident-declare>
  (mop/init
    (mop/class name: "Core::AST::Compiler::Ident::Callable")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:         "name"
              init-arg:     "name"
              reader:       "name"
              is-required:  #t)
            (mop/attribute
              name:         "variable"
              init-arg:     "variable"
              reader:       "variable"
              is-required:  #t))
      (call add-methods:
            (mop/method
              name: "get-value"
              code: (unwrap-pos-args
                      (lambda (self)
                        (pryll:object-data self "variable")))))
      (call finalize:))))

(define (context-identifier ast var)
  (cond ((or (pryll:isa? ast <pryll:ast-subroutine>)
             (pryll:isa? ast <pryll:ast-function>)
             (pryll:isa? ast <pryll:ast-module>))
         (pryll:make <ident-declare>
                     name:      (to-name (pryll:invoke ast "name"))
                     variable:  var))
        (else (error "Unable to declare" ast))))

(define <context>
  (mop/init
    (mop/class name: "Core::AST::Compiler::Context")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:       "parent"
              reader:     "parent"
              init-arg:   "parent")
            (mop/attribute
              name:       "prepared"
              default:    (lambda args (mkhash)))
            (mop/attribute
              name:       "return"
              reader:     "return"
              writer:     "set-return")
            (mop/attribute
              name:       "namespace"
              reader:     "namespace"
              init-arg:   "namespace")
            (mop/attribute
              name:       "identifiers"
              init-arg:   "identifiers"
              default:    (lambda args (mkhash)))
            (mop/attribute
              name:       "special-variables"
              init-arg:   "special-variables"
              default:    (lambda args (mkhash)))
            (mop/attribute
              name:       "variables"
              default:    (lambda args (mkhash))))
      (call add-methods:
            (mop/method
              name: "find-special"
              code: (unwrap-pos-args
                      (lambda (self name)
                        (let ((special (pryll:object-data
                                         self
                                         "special-variables")))
                          (if (hash-table-exists? special name)
                            ((hash-table-ref special name))
                            (call-parent self "find-special" name))))))
            (mop/method
              name: "predeclare"
              code: (unwrap-pos-args
                      (lambda (self ast)
                        (let* ((name (to-name (pryll:invoke ast "name")))
                               (var (compile/genvar name))
                               (var-none (compile/genvar 'none)))
                          (dbg "predeclare " name)
                          (ensure-new-identifier self ast)
                          (hash-table-set!
                            (pryll:object-data self "identifiers")
                            name
                            (context-identifier ast var))
                          `(define ,var (void))))))
            (mop/method
              name: "find-identifier"
              code: (unwrap-pos-args
                      (lambda (self name)
                        (if (hash-table-exists?
                              (pryll:object-data self "identifiers")
                              name)
                          (hash-table-ref
                            (pryll:object-data self "identifiers")
                            name)
                          (call-parent self "find-identifier" name)))))
            (mop/method
              name: "find-namespace"
              code: (unwrap-pos-args
                      (lambda (self)
                        (let ((ns (pryll:object-data self "namespace")))
                          (if (not-void? ns)
                            ns
                            (call-parent self "find-namespace"))))))
            (mop/method
              name: "find-return"
              code: (unwrap-pos-args
                      (lambda (self)
                        (let ((ret (pryll:object-data self "return")))
                          (if (not-void? ret)
                            ret
                            (call-parent self "find-return"))))))
            (mop/method
              name: "find-variable"
              code: (unwrap-pos-args
                      (lambda (self name)
                        (let ((vars (pryll:object-data self "variables"))
                              (parent (pryll:object-data self "parent")))
                          (if (hash-table-exists? vars name)
                            (hash-table-ref vars name)
                            (if parent
                              (pryll:invoke
                                parent
                                "find-variable"
                                (list name))
                              #f))))))
            (mop/method
              name: "commit-variables"
              code: (unwrap-pos-args
                      (lambda (self)
                        (hash-table-merge!
                          (pryll:object-data self "variables")
                          (pryll:object-data self "prepared"))
                        (hash-table-clear!
                          (pryll:object-data self "prepared")))))
            (mop/method
              name: "prepare-variable"
              code: (unwrap-pos-args
                      (lambda (self var)
                        (ensure-new-var self var)
                        (hash-table-set!
                          (pryll:object-data self "prepared")
                          (pryll:invoke var "name")
                          var))))
            (mop/method
              name: "add-variable"
              code: (unwrap-pos-args
                      (lambda (self var)
                        (ensure-new-var self var)
                        (hash-table-set!
                          (pryll:object-data self "variables")
                          (pryll:invoke var "name")
                          var)))))
      (call finalize:))))

(define (subcontext/customized ctx . args)
  (apply pryll:make
         <context>
         parent: ctx
         args))

(define (compile/declaration ctx ast source)
  `(set! ,(pryll:invoke
            (pryll:invoke
              ctx
              "find-identifier"
              (list (to-name (pryll:invoke ast "name"))))
            "variable")
     ,source))

(define (compile/with-namespace ctx ns proc)
  (let ((nsctx (subcontext/customized ctx namespace: ns)))
    (proc nsctx)))

(define (subcontext ctx)
  (pryll:make <context>
              parent:    ctx
              variables: (mkhash)))

(define root-env
  (pryll:make <context>
              parent:       (void)
              identifiers:  (alist->hash-table
                              (list
                                (cons "say"
                                      (pryll:make
                                        <ident-declare>
                                        name: "say"
                                        variable: 'func/say))
                                (cons "print"
                                      (pryll:make
                                        <ident-declare>
                                        name: "print"
                                        variable: 'func/print))))
              variables:    (mkhash)))

(define (ast->code ast)
  (let ((ctx (pryll:make <context>
                         parent:    root-env
                         variables: (mkhash))))
    `(begin
       ,(pryll:invoke
          ast
          "compile"
          (list ctx)))))
