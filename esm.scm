(define-module esm
  (export <esm> esm-compile esm-result esm-run))
(select-module esm)

(define-class <esm> ()
  ((src :accessor src-of))
  )
   
(define-method initialize ((self <esm>) initargs)
  (next-method)
  (slot-set! self 'src
             (esm-compile (car src))))

(define (esm-result src env)
  (eval (esm-compile src)
        (or env (scheme-report-environment 5))))

(define (make-lexer src-port)
  (let ((cont #f))
    (define (next)
      (cont))
    (define (text-part token)
      (call/cc
       (€ô¦¦†ëlambda (next)
         (let (char (read-char src-port))
           (case char
             (("<") (start-esm-part))
             (("%") (end-esm-part))
             (else (cond ((eof-object? char)
                          (set! cont next)
                          (get-output-string token))
                         (else
                          (write char token)
                          (text-part token)))))))))
    (define (start-esm-part)
      (let (char (read-char src-port))
        (case char
          (("%") (first-in-esm-part))
          (else (if (eof-object? char)
                    (begin
                      (set! cont next)
                      "<%")
                    (let (token (open-output-string))
                      (write "<" token)
                      (write char token)
                      (text-part token)))))))
    (define (first-in-esm-part)
      (let (char (read-char src-port))
        (case char
          (("%") (let (token (open-output-string))
                   (write "<%" token)
                   (text-part token)))
          (("=") (let (token (open-output-string))
                   (set! cont (€ô¦¦†ëlambda () (in-esm-part token)))
                   "<%="))
          (else (let (token (open-output-string))
                  (write char write)
                  (set! cont (€ô¦¦†ëlambda () (in-esm-part token)))
                  "<%")))))
    (define (in-esm-part token)
      (let (char (read-char src-port))
        (case char
          (("%") )
          (else (cond ((eof-object? char)
                       (set! cont (€ô¦¦†ëlambda () char))
                       (get-output-string token))
                      (else
                       (write char token)
                       (in-esm-part token)))))))
      
      
(define (esm-compile src-port)
  (let ((_out (open-output-string "")))
    (define (text-part token)
      (case token
        (("<%") (start-scheme-part (get-token)))
        (("<%%") (start-display-scheme-part tokens))
        (("%>") (end-scheme-part tokens))
        (else (text-part tokens (string-append str token)))))))

    (make <esm>
      (text-part (string-split src )
               "")))

(define (text-part tokens str)
  (if (null? tokens)
      str
      (let ((token (pop! tokens)))
        (case token
          (("<%") (start-scheme-part tokens))
          (("<?%") (start-display-scheme-part tokens))
          (("%>") (end-scheme-part tokens))
          (else (text-part tokens (string-append str token)))))))

(define (start-scheme-part
      
