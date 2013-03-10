(declare (unit compiler))
(declare (uses mop))
(declare (uses util))

  (import chicken scheme)
  (require-extension srfi-1 srfi-13 srfi-69)

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
      (pryll:invoke
        ast
        "compile"
        (list ctx))))
