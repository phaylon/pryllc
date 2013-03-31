(declare (unit cmd/build))
(declare (uses config errors parser mop))

(require-extension extras posix utils files srfi-1)
(import chicken scheme)

(define runtime-uses
  '(util
    mop
    exceptions
    errors
    stack
    meta/number
    meta/string
    meta/array
    meta/hash
    meta/void
    nether
    primitives
    runtime
    ))

(define-inline (mtime filename)
  (if (file-exists? filename)
    (file-modification-time filename)
    0))

(define-inline (guard-change source target proc)
  (if (> (mtime source)
         (mtime target))
    (proc)
    #t))

(define-inline (path-for-object filename)
  (conc "build/lib/" filename ".o"))

(define-inline (path-for-scheme filename)
  (conc "build/lib/" filename ".scm"))

(define-inline (path-for-source filename)
  (conc filename ".pryll"))

(define-inline (path-for-meta filename)
  (conc "build/lib/" filename ".meta"))

(define-inline (file-copy/guarded source target)
  (guard-change
    source
    target
    (lambda (from to)
      (print "syncing ~a" to)
      (file-copy from to #t))))

(define-inline (build-meta config)
  (print "preparing build/")
  (create-directory "build/lib" #t)
  (file-copy/guarded "build.conf" "build/build.meta"))

(define-inline (contains str ls)
  (< 0 (length (filter (lambda (item)
                         (string=? item str))
                       ls))))

(define-inline (unique ls)
  (define (eliminate done rest)
    (if (null? rest)
      done
      (let ((next (car rest)))
        (if (contains next done)
          (eliminate done (cdr rest))
          (eliminate (cons next done) (cdr rest))))))
  (reverse (eliminate '() ls)))

(define-inline (find-build-files config)
  (unique
    (append
      (cdr (assoc 'lib-includes config)))))

(define-inline (ensure-exists filename)
  (or (file-exists? filename)
      (pryll:err <pryll:error-build>
                 message: (sprintf "File '~a' does not exist"
                                   filename))))

(define-inline (ensure-file-directory filename)
  (let ((dir (pathname-directory filename)))
    (create-directory dir #t)))

(define (compile-source filename)
  (printf "building ~a\n" filename)
  (let ((source-path (path-for-source filename))
        (scheme-path (path-for-scheme filename))
        (object-path (path-for-object filename))
        (meta-path   (path-for-meta filename)))
    (ensure-exists source-path)
    (let* ((ast (source->ast source-path (read-all source-path)))
           (doc (ast->code ast)))
      ;(pretty-print ast)
      ;(pretty-print (pryll:invoke ast "debug-dump"))
      ;(pretty-print doc)
      (ensure-file-directory scheme-path)
      (with-output-to-file
        scheme-path
        (lambda ()
          (write `(declare (uses ,@runtime-uses)))
          (write doc)))
      (system* "csc -unit ~a -c ~a -o ~a"
               (qs filename)
               (qs scheme-path)
               (qs object-path))
      )))

(define (cmd/build program args)
  (let ((config (read-build-config "./build.conf")))
    (build-meta config)
    (let ((files (find-build-files config)))
      (for-each compile-source files))))
