(define (esm-result src . env)
  (let ((result (esm-compile
                 (if (string? src)
                     (open-input-string src)
                     src))))
    (eval (read (open-input-string result))
          (if (null? env)
              (scheme-report-environment 5)
              (car env)))))

(define (esm-run src . env)
  (display (apply esm-result src env)))

(define-macro (make-token)
  '(open-output-string))

(define-macro (write-to-token value token)
  `(display ,value ,token))

(define-macro (token->string token)
  `(get-output-string ,token))

(define-macro (return-token token)
  `(let ((value (token->string ,token)))
     (if (string=? value "")
         (do-action)
         value)))

(define-macro (make-previous-value token)
  `(lambda () (return-token ,token)))

(define-macro (get-previous-value previous-value)
  `(,previous-value))

(define-macro (make-backtrack token next-action)
  `(lambda (get-string)
     (write-to-token get-string ,token)
     (,next-action ,token)))

(define-macro (do-backtrack backtrack get-string)
  `(,backtrack get-string))

(define-macro (set-action! . actions)
  `(set! action (lambda () ,@actions)))

(define-macro (init-action)
  '(define action #f))

(define-macro (do-action)
  '(action))

(define (make-lexer src-port)
  (define (text-part token)
    (let ((char (read-char src-port)))
      (case char
        ((#\<) (start-esm-part (make-previous-value token)
                               (make-backtrack token text-part)))
        ((#\%) (end-esm-part (make-previous-value token)
                             (make-backtrack token text-part)))
        ((#\newline)
         (set-action! (text-part (make-token)))
         (write-to-token char token)
         (return-token token))
        (else (cond ((eof-object? char)
                     (set-action! char)
                     (return-token token))
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
                     (set-action! (esm-part (make-token)))
                     "<%=")
                    (get-previous-value previous-value))
                   ((#\;)
                    (set-action!
                     (set-action! (comment-part (make-token)))
                     "<%;")
                    (get-previous-value previous-value))
                   (else
                    (set-action!
                     (set-action!
                      (let ((token (make-token)))
                        (if (not (eof-object? next-char))
                            (write-to-token next-char token))
                        (esm-part token)))
                     "<%")
                    (get-previous-value previous-value)))))
        (else (do-backtrack backtrack
                            (if (eof-object? char)
                                "<"
                                (string-append "<" (string char))))))))

  (define (esm-part token)
    (let ((char (read-char src-port)))
      (case char
        ((#\<) (start-esm-part (make-previous-value 
                                (error "bad esm: esm part is nested."))
                               (make-backtrack token esm-part)))
        ((#\%) (end-esm-part (make-previous-value token)
                             (make-backtrack token esm-part)))
        ((#\newline)
         (set-action! (esm-part (make-token)))
         (write-to-token char token)
         (return-token token))
        (else (if (eof-object? char)
                  (error "bad esm" (get-output-string token))
                  (begin
                    (write-to-token char token)
                    (esm-part token)))))))

  (define (comment-part token)
    (let ((char (read-char src-port)))
      (case char
        ((#\%) (end-esm-part (make-previous-value token)
                             (make-backtrack token comment-part)))
        ((#\newline)
         (set-action! (comment-part (make-token)))
         (write-to-token char token)
         (return-token token))
        (else (if (eof-object? char)
                  (error "bad esm" (get-output-string token))
                  (begin
                    (write-to-token char token)
                    (comment-part token)))))))

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

  (init-action)
  (set-action! (text-part (make-token))) ; first action
  (lambda () (do-action)) ; return procedure which do next action
  )

; (define-syntax string-case
;   (syntax-rules (test else =>)
;     ((string-case '()))
;     ((string-case target ((expr ...) action ...) ...)
;      (cond ((or (string=? target expr) ...)
;             action ...)
;            ...))
;     ))
(use srfi-1) ;; for extended member

(define-syntax string-case-sub
  (syntax-rules (else)
    ((_ obj)  #f)
    ((_ obj (else expr ...)) (begin expr ...))
    ((_ obj ((str ...) expr ...) clause ...)
     (if (member obj '(str ...) string=?)
       (begin expr ...)
       (string-case obj clause ...)))
    ((_ obj other . more) (syntax-error "bad clause in string-case" other))))

(define-syntax string-case
  (syntax-rules (else)
    ((_ obj clause ...)
     (let ((tmp obj))
       (string-case-sub tmp clause ...)))))

(define (esm-compile src-port)
  (let ((lexer (make-lexer src-port))
        (result (open-output-string)))

    (define (line? string)
      (eqv? #\newline
            (string-ref string
                        (- (string-length string)
                           1))))

    (define (text-part token first?)
      (cond ((eof-object? token)
             (if (not first?) (display ") " result))
             (display "(print (get-output-string _out)))" result)
             (get-output-string result))
            ((string=? token "<%")
             (if (not first?) (display ") " result))
             (esm-part (lexer)))
            ((string=? token "<%=")
             (if (not first?) (display ") " result))
             (display-esm-part (lexer)))
            ((string=? token "<%;")
             (if (not first?) (display ") " result))
             (comment-part (lexer)))
            ((string=? token "%>") (error "bad esm"))
            (else
             (if first? (display " (begin " result))
             (display " (display " result)
             (write token result)
             (display " _out) " result)
             (if (line? token) (display #\newline result))
             (text-part (lexer) #f))))

    (define (esm-part token)
      (cond ((eof-object? token) (error "unexcepted end"))
            ((string=? token "<%") (error "nested esm part"))
            ((string=? token "<%=") (error "nested esm part(display)"))
            ((string=? token "<%;") (error "nested esm part(comment)"))
            ((string=? token "%>") (text-part (lexer) #t))
            (else
             (display token result)
             (esm-part (lexer)))))

    (define (comment-part token)
      (cond ((eof-object? token) (error "unexcepted end"))
;             ((string=? token "<%") (error "nested esm part"))
;             ((string=? token "<%=") (error "nested esm part(display)"))
;             ((string=? token "<%;") (error "nested esm part(comment)"))
            ((string=? token "%>") (text-part (lexer) #t))
            (else
             (display ";;" result)
             (display token result)
             (if (not (line? token))
                 (display #\newline result)) ;; changed line number. umm.
             (comment-part (lexer)))))

    (define (display-esm-part token)
      (cond ((eof-object? token) (error "unexcepted end"))
            ((string=? token "<%") (error "nested esm part"))
            ((string=? token "<%=") (error "nested esm part(display)"))
            ((string=? token "<%;") (error "nested esm part(comment)"))
            ((string=? token "%>") (text-part (lexer) #t))
            (else
             (display " (display " result)
             (display token result)
             (display " _out) " result)
             (if (line? token) (display #\newline result))
             (esm-part (lexer)))))

    (display "(let ((_out (open-output-string))) " result)
    (text-part (lexer) #t)))
