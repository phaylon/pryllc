(load "lib/objects.scm")
(load "lib/util.scm")

(module pryll/compiler
  (ast->code)

  (import chicken scheme)
  (import pryll/objects)
  (import pryll/util)
  (require-extension srfi-1 srfi-13 srfi-69)

  (define <context>
    (pryll:make
      <pryll:meta-class>
      attributes: (named->hash
                    (pryll:attribute/item "parent")
                    (pryll:attribute/item "variables"))
      methods: (named->hash)))

  (define (ast->code ast)
    (let ((ctx (pryll:make <context>
                           parent:  #f
                           variables: (mkhash))))
      (pryll:call-method
        ast
        "compile"
        (list ctx)
        (mkhash))))
)
