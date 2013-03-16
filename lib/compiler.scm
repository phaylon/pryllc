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

(define type/number
  (list '<pryll:meta-number>
        'number?
        (lambda (value)
          (or (number? value)
              (is-call? '(+ - * /) value)))))

(define-inline (type-meta type) (car type))
(define-inline (type-pred type) (cadr type))
(define-inline (type-const type) (caddr type))

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
              name:       "variables"
              default:    (lambda args (mkhash))))
      (call finalize:))))

(define (ast->code ast)
  (let ((ctx (pryll:make <context>
                         parent:    #f
                         variables: (mkhash))))
    `(begin
       ,(pryll:invoke
          ast
          "compile"
          (list ctx)))))
