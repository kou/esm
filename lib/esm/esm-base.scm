(define *esm-version* "0.0.1")

(define (esm-eval compiled-esm . env)
  (let ((esm (esm-read compiled-esm)))
    (eval esm (if (null? env)
                  *esm-default-environment*
                  (car env)))))

(define (esm-read compiled-esm)
  (read (if (string? compiled-esm)
            (open-input-string compiled-esm)
            compiled-esm)))

(define (esm-result src . env)
  (apply esm-eval (esm-compile src) env))

(define (esm-run src . env)
  (display (apply esm-result src env)))

(define-macro (define-esm name filename env . args)
  `(eval (define (,name ,@args)
           ,(esm-read
             (esm-compile (open-input-file filename))))
         ,env))

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
                  (error "bad esm" (token->string token))
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
                  (error "bad esm" (token->string token))
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
         (result (make-result)))

    (define (line? string)
      (eqv? #\newline
            (string-ref string
                        (- (string-length string)
                           1))))

    (define (text-part token first?)
      (if (eof-object? token)
          (begin
            (if (not first?) (write-to-result ") " result))
            (write-to-result (esm-get-output "_out") result)
            (write-to-result ")" result)
            (result->string result))
          (string-case token
            (("<%")
             (if (not first?) (write-to-result ") " result))
             (esm-part (lexer)))
            (("<%=")
             (if (not first?) (write-to-result ") " result))
             (display-esm-part (lexer)))
            (("<%;")
             (if (not first?) (write-to-result ") " result))
             (comment-part (lexer)))
            (("%>") (error "bad esm"))
            (else
             (if first? (write-to-result " (begin " result))
             (esm-output-text token "_out" result)
             (if (line? token) (write-to-result #\newline result))
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
               (write-to-result token result)
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
               (if (line? token) (write-to-result #\newline result))
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
               (esm-output-scheme token "_out" result)
               (if (line? token) (write-to-result #\newline result))
               (esm-part (lexer))))))

      (write-to-result "(let ((_out " result)
      (write-to-result (esm-make-output) result)
      (write-to-result ")) " result)
      (text-part (lexer) #t)))
