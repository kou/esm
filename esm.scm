(define-module esm
  (export <esm> esm-compile esm-result esm-run test-lex))
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

(define-macro (make-token)
  '(open-output-string))

(define-macro (write-to-token value token)
  `(display ,value ,token))
;   `(if (char? ,value)
;        (write-char ,value ,token)
;        (write ,value ,token)))

(define-macro (token->string token)
  `(get-output-string ,token))

(define-macro (make-previous-value token)
  `(lambda () (token->string ,token)))

(define-macro (get-previous-value previous-value)
  `(,previous-value))

(define-macro (make-backtrack token next-action)
  `(lambda (get-string)
     (write-to-token get-string ,token)
     (,next-action ,token)))

(define-macro (do-backtrack backtrack get-string)
  `(,backtrack get-string))

(define-macro (set-action! . actions)
  `(set! next (lambda () ,@actions)))

(define (make-lexer src-port)
  (let ((next #f))

    (define (text-part token)
      (let ((char (read-char src-port)))
        (case char
          ((#\<) (start-esm-part (make-previous-value token)
                                 (make-backtrack token text-part)))
          ((#\%) (end-esm-part (make-previous-value token)
                               (make-backtrack token text-part)))
          (else (cond ((eof-object? char)
                       (set-action! char)
                       (token->string token))
                      (else
                       (write-to-token char token)
                       (text-part token)))))))

    (define (start-esm-part previous-value backtrack)
      (let ((char (read-char src-port)))
        (case char
          ((#\%) (let ((next-char (read-char src-port)))
                   (case next-char
                     ((#\%) (backtrack "<%"))
                     ((#\=)
                      (set-action!
                       (set-action! (in-esm-part (make-token)))
                       (string-append "<%" (string next-char)))
                      (get-previous-value previous-value))
                     (else
                      (set-action!
                       (set-action!
                        (let ((token (make-token)))
                          (if (not (eof-object? next-char))
                              (write-to-token next-char token))
                          (in-esm-part token)))
                       "<%")
                      (get-previous-value previous-value)))))
          (else (do-backtrack backtrack
                              (if (eof-object? char)
                                  "<"
                                  (string-append "<" (string char))))))))

    (define (in-esm-part token)
      (let ((char (read-char src-port)))
        (case char
          ((#\<) (start-esm-part (make-previous-value 
                                  (error "bad esm: esm part is nested."))
                                 (make-backtrack token in-esm-part)))
          ((#\%) (end-esm-part (make-previous-value token)
                               (make-backtrack token in-esm-part)))
          (else (if (eof-object? char)
                    (error "bad esm" (get-output-string token))
                    (begin
                      (write-to-token char token)
                      (in-esm-part token)))))))

    (define (end-esm-part previous-value backtrack)
      (let ((char (read-char src-port)))
        (case char
          ((#\>)
           (set-action!
            (set-action! (text-part (make-token)))
            "%>")
           (get-previous-value previous-value))
          ((#\%)
           (let ((next-char (read-char src-port)))
             (if (eof-object? next-char)
                 (backtrack "%>")
                 (backtrack (string-append "%" (string next-char))))))
          (else
           (if (eof-object? char)
               (backtrack "%")
               (backtrack (string-append "%" (string char))))))))

    (set-action! (text-part (make-token))) ; first action
    (lambda () (next)) ; return procedure which do next action
    ))

(define (test-lex src-port)
  (let ((lexer (make-lexer src-port)))
    (do ((token (lexer) (lexer)))
        ((eof-object? token) (print "end!!!"))
      (write token)
      (newline))))
      
; (define-syntax string-case
;   (syntax-rules (test else =>)
;     ((string-case '()))
;     ((string-case target ((expr ...) action ...) ...)
;      (cond ((or (string=? target expr) ...)
;             action ...)
;            ...))
;     ))

(define (esm-compile src-port)
  (let ((lexer (make-lexer src-port))
        (_out (open-output-string)))

    (define (text-part token)
      (cond ((eof-object? token) (get-output-string _out))
            ((string=? token "<%") (esm-part (lexer)))
            ((string=? token "<%=") (display-esm-part (lexer)))
            ((string=? token "%>") (error "bad esm"))
            (else 
             (display "(display " _out)
             (write token _out)
             (display ") " _out)
             (text-part (lexer)))))

    (define (esm-part token)
      (cond ((eof-object? token) (error "unexcepted end"))
            ((string=? token "<%") (error "nested esm part"))
            ((string=? token "<%=") (error "nested esm part(display)"))
            ((string=? token "%>") (text-part (lexer)))
            (else
             (display token _out)
             (esm-part (lexer)))))

    (define (display-esm-part token)
      (cond ((eof-object? token) (error "unexcepted end"))
            ((string=? token "<%") (error "nested esm part"))
            ((string=? token "<%=") (error "nested esm part(display)"))
            ((string=? token "%>") (text-part (lexer)))
            (else
             (display "(display " _out)
             (display token _out)
             (display ") " _out)
             (esm-part (lexer)))))

    (text-part (lexer))))
