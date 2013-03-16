(declare (unit errors))
(declare (uses exceptions))
(declare (uses mop))

(define (pryll:err class . args)
  (pryll:invoke
    (pryll:invoke class "new" (list) (apply phash args))
    "throw"))

(define <pryll:role-throwable>
  (mop/init
    (mop/role name: "Throwable")
    (lambda (call)
      (call add-method:
            (mop/method
              name: "get-full-message"
              code: (lambda (pos nam)
                      (pryll:invoke (car pos) "get-message"))))
      (call add-method:
            (mop/method
              name: "throw"
              code: (lambda (pos nam)
                      (pryll:throw (car pos))))))))

(define <pryll:role-throwable-message>
  (mop/init
    (mop/role name: "Throwable::Message")
    (lambda (call)
      (call add-attribute:
            (mop/attribute
              name:         "message"
              init-arg:     "message"
              reader:       "message"
              is-required:  #t))
      (call add-method:
            (mop/method
              name: "get-message"
              code: (lambda (pos nam)
                      (pryll:object-data (car pos) "message")))))))

(define <pryll:role-throwable-location>
  (mop/init
    (mop/role name: "Throwable::Location")
    (lambda (call)
      (call add-attribute:
            (mop/attribute
              name:         "location"
              init-arg:     "location"
              reader:       "location"))
      (call add-method-modifier:
            "get-full-message"
            (lambda (pos nam)
              (let* ((orig (car pos))
                     (self (cadr pos))
                     (rest (cddr pos))
                     (loc (pryll:object-data self "location")))
                (conc (orig (append (list self) rest) nam)
                      (if (not-void? loc)
                        (conc "\n\tin "
                              (list-ref loc 0)
                              " line "
                              (list-ref loc 1)
                              " char "
                              (list-ref loc 2))
                        ""))))))))

(define <pryll:role-throwable-stacktrace>
  (mop/init
    (mop/role name: "Throwable::StackTrace")
    (lambda (call)
      (call add-attribute:
            (mop/attribute
              name:    "trace"
              default: (lambda (pos nam)
                         (cdr (pryll:current-stack)))))
      (call add-method-modifier:
            "get-full-message"
            (lambda (pos nam)
              (let* ((orig (car pos))
                     (self (cadr pos))
                     (rest (cddr pos)))
                (conc (orig (append (list self) rest) nam)
                      "\n\nStackTrace:\n"
                      (apply
                        conc
                        (map (lambda (item)
                               (conc "\t" (string-join item " ") "\n"))
                             (pryll:object-data self "trace"))))))))))

(define <pryll:error-type>
  (mop/init
    (mop/class name: "Error::Type")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:        "expected"
              init-arg:    "expected"
              reader:      "expected"
              is-required: #t)
            (mop/attribute
              name:        "received"
              init-arg:    "received"
              reader:      "received"
              is-required: #t))
      (call add-methods:
            (mop/method
              name: "get-message"
              code: (lambda (pos nam)
                      (let* ((self     (car pos))
                             (expected (pryll:name
                                         (pryll:invoke self "expected")))
                             (received (pryll:name
                                         (pryll:invoke self "received"))))
                        (conc
                          "Expected <"
                          expected
                          "> but received <"
                          received
                          ">")))))
      (call add-role:
            <pryll:role-throwable>)
      (call add-role:
            <pryll:role-throwable-location>)
      (call add-role:
            <pryll:role-throwable-stacktrace>)
      (call finalize:))))

(define <pryll:error-syntax>
  (mop/init
    (mop/class name: "Error::Syntax")
    (lambda (call)
      (call add-role:
            <pryll:role-throwable>)
      (call add-role:
            <pryll:role-throwable-message>)
      (call add-role:
            <pryll:role-throwable-location>)
      (call add-role:
            <pryll:role-throwable-stacktrace>)
      (call finalize:))))

;(pryll:err <pryll:error-type>
;           expected: <pryll:meta-attribute>
;           received: <pryll:meta-method>)

