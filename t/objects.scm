(load "lib/mop.scm")
(load "lib/util.scm")
;(load "lib/test.scm")

(import pryll/mop)
(import pryll/util)
;(import pryll/test)

(define meta-name
  (pryll:invoke <pryll:meta-class> "name"))

(dbg "--- Name: " meta-name)

(define test-class
  (pryll:invoke <pryll:meta-class>
                "new"
                (list)
                (phash name: "Test::Class")))

(dbg "--- Test Class " test-class)

(define test-method
  (pryll:invoke
    <pryll:meta-method>
    "new"
    (list)
    (phash name: "foo"
           code: (lambda (pos nam) 23))))

(dbg "--- Test Method " test-method)

(define test-attr
  (pryll:invoke
    <pryll:meta-attribute>
    "new"
    (list)
    (phash name: "bar"
           default: (lambda args 17)
           reader: "bar")))

(dbg "--- Test Attribute " test-attr)

(pryll:invoke
  test-class
  "add-method"
  (list test-method))

(dbg "--- Added Test Method")

(pryll:invoke
  test-class
  "add-attribute"
  (list test-attr))

(dbg "--- Added Test Attribute")

(pryll:invoke test-class "finalize")

(dbg "--- Test Name: " (pryll:invoke test-class "name"))

(define test-object
  (pryll:invoke test-class "new"))

(dbg "--- Test Object: " test-object)

(dbg "--- Result: " (pryll:invoke test-object "foo"))

(dbg "--- Attribute: " (pryll:invoke test-object "bar"))

;(t/done)
