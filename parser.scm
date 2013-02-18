
(module parsing (source->ast)
  (import scheme)
  (import srfi-1)
  (import chicken)
  (require-extension lalr-driver irregex)
  
  (import ast)

  (include "grammar.yy.scm")

  (define token-patterns
    '((INT          (: bos ($ (+ (/ "09")))))
      (SEMICOLON    (: bos ($ (+ ";"))))))

  (define (say . ls)
    (map display ls)
    (newline))

  (define (unempty lines)
    (if (null? lines)
      '("")
      lines))

  (define (pryll-location->source-location loc)
    (make-source-location (car loc) (cadr loc) (caddr loc) -1 -1))

  (define (source->token-iterator name body)
    (define get-location
      (let ((original body))
        (lambda ()
          (let* ((removed (irregex-replace `(: ,body eos) ""))
                 (lines   (unempty (irregex-split 'newline removed)))
                 (last    (car (reverse lines)))
                 (lnum    (length lines))
                 (cnum    (+ 1 (string-length last))))
            `(,name ,lnum ,cnum)))))
    (define (next-token patterns)
      (if (null? patterns)
        #f
        (let* ((token   (car patterns))
               (rest    (cdr patterns))
               (type    (car token))
               (pattern (cadr token)))
          (say "type: " type)
          (say "pattern: " pattern)
          (let ((match (irregex-search pattern body)))
            (if match
              (let ((str (irregex-match-substring match)))
                (set! body (irregex-replace pattern body ""))
                (say "rest body: " body)
                (let ((location (get-location)))
                  (make-lexical-token
                    type
                    (pryll-location->source-location location)
                    `(,str ,location))))
              (next-token rest))))))
    (lambda ()
      (say "--- get token")
      (if (zero? (string-length body))
        '*eoi*
        (let ((found (next-token token-patterns)))
          found))))

  (define (source->ast name body)
    (parser
      (source->token-iterator name body)
      (lambda args
        (say args)
        (error "An error occured")))))
