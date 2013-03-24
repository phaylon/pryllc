(declare (unit ast/util))
(declare (uses mop))

(import chicken scheme)
(require-extension irregex)

(define (token-value token) (car token))
(define (token-location token) (cadr token))

(define (dump item)
  (pryll:invoke
    item
    "debug-dump"))

(define (dump-slot obj slot)
  (dump (pryll:object-data obj slot)))

(define (dump-method proc)
  (mop/method
    name: "debug-dump"
    code: (lambda (pos nam)
            (proc (car pos)))))

(define (compile-method proc)
  (mop/method
    name: "compile"
    code: (lambda (pos nam)
            (proc (car pos) (cadr pos)))))

(define (compile ctx item)
  (pryll:invoke item "compile" (list ctx) (mkhash)))

(define (attr/item name)
  (mop/attribute
    name: name
    init-arg: name
    reader: name))


