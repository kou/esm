(define *esm-default-environment* (scheme-report-environment 5))

(define (esm-eval compiled-esm . env)
  (let ((esm (read (if (string? compiled-esm)
                       (open-input-string compiled-esm)
                       compiled-esm))))
    (eval esm (if (null? env)
                  *esm-default-environment*
                  (car env)))))

(define (esm-result src . env)
  (apply esm-eval (esm-compile src) env))

(define (esm-run src . env)
  (display (apply esm-result src env)))

(define-macro (define-esm name src env . args)
  `(define (,name ,@args)
     (display (apply esm-eval
                     ,(esm-compile src)
                     ,env))))

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

(define (include? key sequence predicate)
  (and (not (null? sequence))
       (if (predicate key (car sequence))
           #t
           (include? key (cdr sequence) predicate))))

(define-syntax string-case
  (syntax-rules (else)
    ((_ key) #f)
    ((_ key (else expr ...)) (begin expr ...))
    ((_ key ((str ...)) clause ...)
     (syntax-error "expressions are not found in string-case" ((str ...))))
    ((_ key ((str ...) expr ...) clause ...)
     (let ((evaled-key key))
       (if (include? evaled-key '(str ...) string=?)
           (begin expr ...)
           (string-case evaled-key clause ...))))
    ((_ other . more)
     (syntax-error "bad clause in string-case" other))))

(define (esm-compile src)
  (let* ((src-port (if (string? src)
                       (open-input-string src)
                       src))
         (lexer (make-lexer src-port))
         (result (open-output-string)))

    (define (line? string)
      (eqv? #\newline
            (string-ref string
                        (- (string-length string)
                           1))))

    (define (text-part token first?)
      (if (eof-object? token)
          (begin
            (if (not first?) (display ") " result))
            (display "(print (get-output-string _out)))" result)
            (get-output-string result))
          (string-case token
            (("<%")
             (if (not first?) (display ") " result))
             (esm-part (lexer)))
            (("<%=")
             (if (not first?) (display ") " result))
             (display-esm-part (lexer)))
            (("<%;")
             (if (not first?) (display ") " result))
             (comment-part (lexer)))
            (("%>") (error "bad esm"))
            (else
             (if first? (display " (begin " result))
             (display " (display " result)
             (write token result)
             (display " _out) " result)
             (if (line? token) (display #\newline result))
             (text-part (lexer) #f)))))

      (define (esm-part token)
        (if (eof-object? token)
            (error "unexcepted end")
            (string-case token
              (("<%") (error "nested esm part"))
              (("<%=") (error "nested esm part(display)"))
              (("<%;") (error "nested esm part(comment)"))
              (("%>") (text-part (lexer) #t))
              (else
               (display token result)
               (esm-part (lexer))))))

      (define (comment-part token)
        (if (eof-object? token)
            (error "unexcepted end")
            (string-case token
              ;; (("<%") (error "nested esm part"))
              ;; (("<%=") (error "nested esm part(display)"))
              ;; (("<%;") (error "nested esm part(comment)"))
              (("%>") (text-part (lexer) #t))
              (else
               (if (line? token) (display #\newline result))
               (comment-part (lexer))))))

      (define (display-esm-part token)
        (if (eof-object? token)
            (error "unexcepted end")
            (string-case token
              (("<%") (error "nested esm part"))
              (("<%=") (error "nested esm part(display)"))
              (("<%;") (error "nested esm part(comment)"))
              (("%>") (text-part (lexer) #t))
              (else
               (display " (display " result)
               (display token result)
               (display " _out) " result)
               (if (line? token) (display #\newline result))
               (esm-part (lexer))))))

      (display "(let ((_out (open-output-string))) " result)
      (text-part (lexer) #t)))
