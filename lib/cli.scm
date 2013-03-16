(declare (unit cli))
(declare (uses util))

(require-extension irregex)

(define (cli/option-value options name)
  (cadr (assoc name options)))

(define (cli/option-exists? options name)
  (if (assoc name options)
    #t
    #f))

(define (cli/parse spec proc)
  (define (spec->alist spec alist)
    (if (null? spec)
      alist
      (let* ((next (car spec))
             (names (car next))
             (type (cadr next))
             (primary-name (car names)))
        (spec->alist
          (cdr spec)
          (append
            (map (lambda (name)
                   (let* ((str-name (symbol->string name))
                          (option (if (< 1 (string-length str-name))
                                    (conc "--" str-name)
                                    (conc "-" str-name))))
                     (list option type primary-name)))
                 names)
            alist)))))
  (define (option-name option)
    (list-ref option 2))
  (define (option-type option)
    (list-ref option 1))
  (define spec-list (spec->alist spec '()))
  (define (option? value)
    (irregex-match? '(: "-" (* any)) value))
  (define (find-option arg)
    (assoc arg spec-list))
  (define (flag-option? opt-spec)
    (equal? (option-type opt-spec) 'b))
  (define (item-option? opt-spec)
    (equal? (option-type opt-spec) 's))
  (define (list-option? opt-spec)
    (equal? (option-type opt-spec) 'l))
  (define (options-with-flag options name)
    (let ((curr (assoc name options)))
      (if (not curr)
        (append options
                (list (list name #t)))
        options)))
  (define (options-with-item options name value)
    (let ((curr (assoc name options)))
      (if (not curr)
        (append options
                (list (list name value)))
        (error "Option can only be specified once" name))))
  (define (options-with-list options name value)
    (let ((curr (assoc name options)))
      (if (not curr)
        (append options
                (list (list name (list value))))
        (begin
          (set! (list-ref curr 1)
            (append (list-ref curr 1)
                    (list value)))
          options))))
  (define (traverse args options rest)
    (if (null? args)
      (proc options rest)
      (let* ((next-arg (car args)))
        (if (option? next-arg)
          (let* ((opt-spec (find-option next-arg)))
            (if (not opt-spec)
              (error "Unknown CLI option" next-arg)
              (cond ((flag-option? opt-spec)
                     (traverse (cdr args)
                               (options-with-flag
                                 options
                                 (option-name opt-spec))
                               rest))
                    ((item-option? opt-spec)
                     (traverse (cddr args)
                               (options-with-item
                                 options
                                 (option-name opt-spec)
                                 (cadr args))
                               rest))
                    ((list-option? opt-spec)
                     (traverse (cddr args)
                               (options-with-list
                                 options
                                 (option-name opt-spec)
                                 (cadr args))
                               rest))
                    (else (error "Unable to determine optio type"
                                 opt-spec)))))
          (traverse (cdr args)
                    options
                    (append rest (list next-arg)))))))
  (let ((args (argv)))
    (traverse (cdr args) '() '())))
