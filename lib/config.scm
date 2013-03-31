(declare (unit config))

(require-extension irregex data-structures srfi-1)
(import chicken scheme)

(define (read-config filename)
  (if (file-exists? filename)
    (read-all filename)
    (begin
      (printf "Could not find config file '~a' in current directory\n"
              filename)
      (exit))))

(define rx-alpha
  `(or (/ "az") (/ "AZ")))

(define rx-alpha-num
  `(or ,rx-alpha (/ "09")))

(define rx-ident
  `(: ,rx-alpha
      (* ,rx-alpha-num)
      (* (or "-" ".")
         (+ ,rx-alpha-num))))

(define rx-int
  `(or "0"
       (: (/ "19")
          (* (/ "09")))))

(define rx-version
  `(: ,rx-int
      (* "."
         ,rx-int)))

(define rx-authority
  `(+ (- any whitespace)))

(define rx-ws
  '(+ whitespace))

(define rx-file
  `(+ any))

(define build-config-struct
  `((project.id (: "provides" ,rx-ws
                   ($ ,rx-ident) ,rx-ws
                   "version" ,rx-ws
                   ($ ,rx-version) ,rx-ws
                   "by" ,rx-ws
                   ($ ,rx-authority))
                (name version authority))
    (lib.include (: "lib" ,rx-ws
                    "includes" ,rx-ws
                    ($ ,rx-file))
                 (filename))))

(define (build-config-inflate ls)
  (let ((name #f)
        (version #f)
        (authority #f)
        (lib-includes (list)))
    (for-each (lambda (item)
                (let* ((key (car item))
                       (data (cdr item)))
                  (case key
                    ((project.id)
                     (begin
                       (set! name (cdr (assoc 'name data)))
                       (set! version (cdr (assoc 'version data)))
                       (set! authority (cdr (assoc 'authority data)))))
                    ((lib.include)
                     (begin
                       (set! lib-includes
                         (cons (cdr (assoc 'filename data))
                               lib-includes))))
                    (else
                      (error "Unknown config key" key)))))
              ls)
    (if (not name)
      (begin
        (display (conc "Error: Build config does not "
                       "contain provide declaration\n"))
        (exit)))
    (list (cons 'name name)
          (cons 'version (map string->number (string-split version ".")))
          (cons 'authority authority)
          (cons 'lib-includes (reverse lib-includes)))))

(define (extract-matches captures match done)
  (if (null? captures)
    done
    (let* ((cap (car captures))
           (idx (length done)))
      (extract-matches
        (cdr captures)
        match
        (cons (cons cap (irregex-match-substring match (+ 1 idx)))
              done)))))

(define (parse-config-line struct line)
  (if (null? struct)
    (begin
      (printf "Error: Unable to parse config line:\n~a\n" line)
      (exit))
    (let* ((try (car struct))
           (key (car try))
           (rx  (cadr try))
           (cap (caddr try)))
      (let ((match (irregex-match `(: bos ,rx eos) line)))
        (if match
          (cons key (extract-matches cap match '()))
          (parse-config-line (cdr struct) line))))))

(define (parse-config struct inflate content)
  (let* ((lines (string-split content "\n")))
    (inflate (map (lambda (line)
                    (parse-config-line struct line))
                  lines))))

(define (read-build-config filename)
  (let* ((content (read-config filename)))
    (parse-config
      build-config-struct
      build-config-inflate
      content)))
