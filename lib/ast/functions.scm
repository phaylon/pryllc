(declare (unit ast/functions))
(declare (uses ast/util mop compiler))

(import chicken scheme)

(declare (hide find-module compile-module-function))

(define (find-module self ctx)
  (let ((ident (pryll:invoke ctx "find-special" (vector "$*MODULE"))))
    (if ident
      ident
      (error "Function declaration outside of module declaration"))))

(define (compile-module-function self ctx)
  (let* ((src-mod   (find-module self ctx))
         (loc       (pryll:object-data self "location"))
         (name      (pryll:object-data self "name"))
         (sign      (pryll:object-data self "signature"))
         (block     (pryll:object-data self "block"))
         (var-proc  (compile/genvar 'proc)))
    `(let ((,var-proc
             ,(compile
                ctx
                (pryll:make
                  <pryll:ast-lambda>
                  stack-type:   "function"
                  stack-description: (conc name
                                           " in module "
                                           (pryll:invoke
                                             ctx
                                             "find-namespace"))
                  location:     loc
                  signature:    sign
                  block:        block))))
       ,(compile/declaration ctx self var-proc)
       (pryll:invoke
         ,src-mod
         "add-function"
         (vector
           (pryll:make <pryll:meta-function>
                       name: ,name
                       code: ,var-proc))))))

(define <pryll:ast-function>
  (mop/init
    (mop/class name: "Core::AST::Function")
    (lambda (call)
      (call add-attributes:
            (attr/item "location")
            (attr/item "name")
            (attr/item "signature")
            (attr/item "traits")
            (attr/item "block"))
      (call add-methods:
            (compile-method
              compile-module-function)
            (dump-method
              (lambda (self)
                `(function
                   ,(pryll:object-data self "name")
                   ,(let ((s (pryll:object-data self "signature")))
                      (if s (dump s) 'no-signature))
                   ,@(map dump (pryll:object-data self "traits"))
                   ,(dump-slot self "block")))))
      (call finalize:))))

(define (make-function op name signature traits block)
  (pryll:make <pryll:ast-function>
              location:     (token-location op)
              name:         (pryll:invoke name "value")
              signature:    signature
              traits:       traits
              block:        block))

