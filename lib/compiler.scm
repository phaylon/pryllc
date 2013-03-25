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
  (pryll:isa? item <pryll:ast-subroutine>))

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

(define (ensure-new-callable self ast)
  (let* ((name (pryll:invoke ast "name"))
         (loc (pryll:invoke ast "location"))
         (known (or (hash-table-exists?
                      (pryll:object-data self "callables")
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
              name:       "callables"
              init-arg:   "callables"
              default:    (lambda args (mkhash)))
            (mop/attribute
              name:       "variables"
              default:    (lambda args (mkhash))))
      (call add-methods:
            (mop/method
              name: "predeclare"
              code: (unwrap-pos-args
                      (lambda (self ast)
                        (let* ((name (pryll:invoke ast "name"))
                               (var (compile/genvar name))
                               (var-none (compile/genvar 'none)))
                          (ensure-new-callable self ast)
                          (hash-table-set!
                            (pryll:object-data self "callables")
                            name
                            var)
                          `(define ,var (void))))))
            (mop/method
              name: "find-callable"
              code: (unwrap-pos-args
                      (lambda (self name)
                        (if (hash-table-exists?
                              (pryll:object-data self "callables")
                              name)
                          (hash-table-ref
                            (pryll:object-data self "callables")
                            name)
                          (call-parent self "find-callable" name)))))
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

(define (subcontext ctx)
  (pryll:make <context>
              parent:    ctx
              variables: (mkhash)))

(define root-env
  (pryll:make <context>
              parent:       (void)
              callables:    (alist->hash-table
                              '(("say"      . func/say)
                                ("print"    . func/print)))
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
