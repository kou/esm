(define *esm-version* "0.0.2")

(define *esm-debug-mode* #f)

(define *start-esm-part* '<%)
(define *start-display-esm-part* '<%=)
(define *start-comment-part* (string->symbol "<%;"))
(define *end-esm-part* '%>)

(define (make-reader src-port)
  (let ((buffer ""))
    (lambda (command . args)
      (case command
        ((next)
         (if (string=? buffer "")
             (read-char src-port)
             (let ((char (string-ref buffer 0)))
               (set! buffer
                     (substring buffer 1 (string-length buffer)))
               char)))
        ((unput)
         (set! buffer
               (string-append buffer
                              (car args))))
        (else (error "unknown command: " command))))))


(define-macro (set-action! . actions)
  `(set! action (lambda () ,@actions)))

(define-macro (do-action)
  '(action))

(define (make-lexer src-port)

  (let ((reader (make-reader src-port)))

    (define (text-part token)
      (let ((char (reader 'next)))
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
      (let ((char (reader 'next)))
        (case char
          ((#\%) (let ((next-char (reader 'next)))
                   (case next-char
                     ((#\%) (do-backtrack backtrack "<%" #f))
                     ((#\=)
                      (set-action!
                       (set-action! (esm-part (make-token)))
                       *start-display-esm-part*)
                      (get-previous-value previous-value))
                     ((#\;)
                      (set-action!
                       (set-action! (comment-part (make-token)))
                       *start-comment-part*)
                      (get-previous-value previous-value))
                     (else
                      (set-action!
                       (set-action!
                        (let ((token (make-token)))
                          (if (not (eof-object? next-char))
                              (write-to-token next-char token))
                          (esm-part token)))
                       *start-esm-part*)
                      (get-previous-value previous-value)))))
          (else
           (do-backtrack backtrack
                         "<"
                         (if (eof-object? char)
                             #f
                             (string char)))))))

    (define (esm-part token)
      (let ((char (reader 'next)))
        (case char
          ((#\<) (start-esm-part (make-previous-value token)
                                 ;; don't report error
                                 ;; (error "bad esm: esm part is nested.") 
                                 (make-backtrack token esm-part)))
          ((#\%) (end-esm-part (make-previous-value token)
                               (make-backtrack token esm-part)))
          ((#\newline)
           (set-action! (esm-part (make-token)))
           (write-to-token char token)
           (return-token token))
          (else (if (eof-object? char)
                    (error "bad esm: " (token->string token))
                    (begin
                      (write-to-token char token)
                      (esm-part token)))))))

    (define (comment-part token)
      (let ((char (reader 'next)))
        (case char
          ((#\%) (end-esm-part (make-previous-value token)
                               (make-backtrack token comment-part)))
          ((#\newline)
           (set-action! (comment-part (make-token)))
           (write-to-token char token)
           (return-token token))
          (else (if (eof-object? char)
                    (error "bad esm: " (token->string token))
                    (begin
                      (write-to-token char token)
                      (comment-part token)))))))

    (define (end-esm-part previous-value backtrack)
      (let ((char (reader 'next)))
        (case char
          ((#\>)
           (set-action!
            (set-action! (text-part (make-token)))
            *end-esm-part*)
           (get-previous-value previous-value))
          ((#\%)
           (let ((next-char (reader 'next)))
             (cond ((eof-object? next-char)
                    (do-backtrack backtrack "%%" #f))
                   ((equal? next-char #\>)
                    (do-backtrack backtrack "%>" #f))
                   (else
                    (do-backtrack backtrack
                                  "%"
                                  (string-append "%"
                                                 (string next-char)))))))
          (else
           (do-backtrack backtrack
                             "%"
                             (if (eof-object? char)
                                 #f
                                 (string char)))))))

    (define action #f)
    
    (set-action! (text-part (make-token))) ; first action
    (lambda () (do-action)) ; return procedure which do next action
    ))

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

(define-syntax token-case
  (syntax-rules (else)
    ((_ key) #f)
    ((_ key (else expr ...)) (begin expr ...))
    ((_ key ((token ...)) clause ...)
     (syntax-error "expressions are not found in token-case" ((token ...))))
    ((_ key ((token ...) expr ...) clause ...)
     (let ((evaled-key key))
       (if (include? evaled-key `(,token ...) eqv?)
           (begin expr ...)
           (token-case evaled-key clause ...))))
    ((_ other . more)
     (syntax-error "bad clause in token-case" other))))

(define (esm-compile src)
  (let* ((src-port (if (string? src)
                       (open-input-string src)
                       src))
         (lexer (make-lexer src-port))
         (result (make-result))
         (output-port-name "_out")
         (line 1))

    (define (report-error message)
      (if *esm-debug-mode*
          (print (result->string result)))
      (error (string-append "esm compile error at line "
                            (number->string line)
                            ": "
                            message)))

    (define (found-line)
      (set! line (+ line 1)))

    (define (line? string)
      (eqv? #\newline
            (string-ref string
                        (- (string-length string)
                           1))))

    (define (text-part token first?)
      (if (eof-object? token)
          (begin
            (if (not first?) (write-to-result ") " result))
            (write-to-result (esm-get-output output-port-name) result)
            (write-to-result ")" result)
            (if *esm-debug-mode*
                (print (result->string result)))
            (result->string result))
          (token-case token
            ((*start-esm-part*)
             (if (not first?) (write-to-result ") " result))
             (esm-part (lexer)))
            ((*start-display-esm-part*)
             (if (not first?) (write-to-result ") " result))
             (display-esm-part (lexer) #t))
            ((*start-comment-part*)
             (if (not first?) (write-to-result ") " result))
             (comment-part (lexer)))
            ((*end-esm-part*)
             (report-error "'%>' is appeared in text part."))
            (else
             (if first? (write-to-result " (begin " result))
             (esm-output-text token output-port-name result)
             (if (line? token)
                 (begin
                   (found-line)
                   (write-to-result #\newline result)))
             (text-part (lexer) #f)))))

      (define (esm-part token)
        (if (eof-object? token)
            (report-error "unexcepted end in esm part.")
            (token-case token
              ((*start-esm-part*)
               (report-error "nested esm part found"))
              ((*start-display-esm-part*)
               (report-error "display esm part found in esm part."))
              ((*start-comment-part*)
               (report-error "comment part found in esm part."))
              ((*end-esm-part*)
               (text-part (lexer) #t))
              (else
               (write-to-result token result)
               (esm-part (lexer))))))

      (define (comment-part token)
        (if (eof-object? token)
            (report-error "unexcepted end found in comment part.")
            (token-case token
              ;; ((*start-esm-part*) (error "nested esm part"))
              ;; ((*start-display-esm-part*) (error "nested esm part(display)"))
              ;; ((*start-comment-part*) (error "nested esm part(comment)"))
              ((*end-esm-part*) (text-part (lexer) #t))
              (else
               (if (line? token)
                   (begin
                     (found-line)
                     (write-to-result #\newline result)))
               (comment-part (lexer))))))

      (define (display-esm-part token first?)
        (if (eof-object? token)
            (report-error "unexcepted end")
            (token-case token
              ((*start-esm-part*)
               (report-error "esm part found in display esm part."))
              ((*start-display-esm-part*)
               (report-error "nested display esm part found."))
              ((*start-comment-part*)
               (report-error "comment part found in esm display part."))
              ((*end-esm-part*)
               (if (not first?)
                   (esm-output-end output-port-name result))
               (text-part (lexer) #t))
              (else
               (if first?
                   (esm-output-start result))
               (esm-output-scheme-contents token result)
               (if (line? token)
                   (found-line))
               (display-esm-part (lexer)
                                 #f)))))

      (write-to-result (string-append "(let (("
                                      output-port-name
                                      " ")
                       result)
      (write-to-result (esm-make-output) result)
      (write-to-result "))" result)
      (text-part (lexer) #t)))

(define (esm-result src . env)
  (eval `(esm-result* ,src)
        (if (null? env)
            *esm-default-environment*
            (car env))))

(define-macro (esm-result* src)
  (read-from-string (esm-compile src)))

(define (esm-run src . env)
  (display (apply esm-result src env)))

(define-macro (esm-run* src)
  `(display (esm-result* ,src)))

(define-macro (define-esm name filename)
  (let ((args (gensym)))
    `(define (,name . ,args)
       (define (get-param keyword . fall-back)
         (apply get-keyword keyword ,args fall-back))
       (esm-result* ,(open-input-file filename)))))
