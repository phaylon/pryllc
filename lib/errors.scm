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
                      "\n\nStack Trace:\n"
                      (apply
                        conc
                        (map (lambda (item)
                               (conc "\t"
                                     (pryll:format-stack-id item)
                                     "\n"))
                             (pryll:object-data self "trace"))))))))))

(define <pryll:error-generic>
  (mop/init
    (mop/class name: "Error::Generic")
    (lambda (call)
      (call add-roles:
            <pryll:role-throwable>
            <pryll:role-throwable-message>
            <pryll:role-throwable-stacktrace>)
      (call finalize:))))

(define <pryll:role-throwable-rescue>
  (mop/init
    (mop/role name: "Throwable::Rescue")
    (lambda (call)
      (call add-attribute:
            (mop/attribute
              name:     "continuation"
              init-arg: "continuation"))
      (call add-method-modifier:
            "throw"
            (lambda (pos nam)
              (let* ((orig (car pos))
                     (self (cadr pos))
                     (rest (cddr pos)))
                (call/cc
                  (lambda (cont)
                    (if (void? (pryll:object-data self "continuation"))
                      (set! (pryll:object-data self "continuation")
                        cont))
                    (orig (append (list self) rest)
                          nam))))))
      (call add-method:
            (mop/method
              name: "rescue"
              code: (lambda (pos nam)
                      (let ((cont
                              (pryll:object-data self "continuation")))
                        (if (not-void? cont)
                          (cont (cadr pos))
                          (pryll:err
                            <pryll:error-generic>
                            message: (conc "Unable to rescue unthrown <"
                                           (pryll:name
                                             (pryll:meta-for self))
                                           "> exception"))))))))))

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
      (call add-roles:
            <pryll:role-throwable>
            <pryll:role-throwable-location>
            <pryll:role-throwable-stacktrace>)
      (call finalize:))))

(define <pryll:error-coercion>
  (mop/init
    (mop/class name: "Error::Coercion")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:        "from-type"
              init-arg:    "from-type"
              reader:      "from-type"
              is-required: #t)
            (mop/attribute
              name:        "to-type"
              init-arg:    "to-type"
              reader:      "to-type"
              is-required: #t))
      (call add-roles:
            <pryll:role-throwable>
            <pryll:role-throwable-message>)
      (call add-method-modifier:
            "get-message"
            (lambda (pos nam)
              (let* ((orig (car pos))
                     (self (cadr pos))
                     (rest (cddr pos)))
                (conc "Unable to coerce from <"
                      (pryll:name (pryll:invoke self "from-type"))
                      "> to <"
                      (pryll:name (pryll:invoke self "to-type"))
                      "> because:\n\t"
                      (orig (append (list self) rest) nam)))))
      (call add-roles:
            <pryll:role-throwable-location>
            <pryll:role-throwable-stacktrace>)
      (call finalize:))))

(define <pryll:error-unknown-method>
  (mop/init
    (mop/class name: "Error::UnknownMethod")
    (lambda (call)
      (call add-attributes:
            (mop/attribute
              name:         "meta"
              reader:       "meta"
              init-arg:     "meta"
              is-required:  #t)
            (mop/attribute
              name:         "method"
              reader:       "method"
              init-arg:     "method"
              is-required:  #t))
      (call add-methods:
            (mop/method
              name: "get-message"
              code: (lambda (pos nam)
                      (let ((self (car pos)))
                        (sprintf "Method ~s is not available on <~a>"
                                 (pryll:object-data self "method")
                                 (pryll:name
                                   (pryll:object-data self "meta")))))))
      (call add-roles:
            <pryll:role-throwable>
            <pryll:role-throwable-location>
            <pryll:role-throwable-stacktrace>)
      (call finalize:))))

(define <pryll:error-syntax>
  (mop/init
    (mop/class name: "Error::Syntax")
    (lambda (call)
      (call add-roles:
            <pryll:role-throwable>
            <pryll:role-throwable-message>
            <pryll:role-throwable-location>
            <pryll:role-throwable-stacktrace>)
      (call finalize:))))

;(pryll:err <pryll:error-type>
;           expected: <pryll:meta-attribute>
;           received: <pryll:meta-method>)

