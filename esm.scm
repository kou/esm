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

(define (make-lexer src-port)
  (let ((next #f))
    (define (text-part token)
      (let ((char (read-char src-port)))
        (case char
          ((#\<) (start-esm-part (lambda (get-string)
                                   (write get-string token)
                                   (text-part token))))
          ((#\%) (end-esm-part (lambda (get-string)
                                 (write get-string token)
                                 (text-part token))))
          (else (cond ((eof-object? char)
                       (set! next (lambda () char))
                       (get-output-string token))
                      (else
                       (write-char char token)
                       (text-part token)))))))
    (define (start-esm-part backtrack)
      (let ((char (read-char src-port)))
        (case char
          ((#\%) (let ((next-char (read-char src-port)))
                   (case next-char
                     ((#\% #\=)
                      (set! next
                            (lambda ()
                              (in-esm-part (open-output-string))))
                      (string-append "<%" next-char))
                     (else
                      (set! next
                            (lambda ()
                              (let ((token (open-output-string)))
                                (if (not (eof-object? next-char))
                                    (write-char next-char token))
                                (in-esm-part token))))
                      "<%"))))
          (else (if (eof-object? char)
                    (backtrack "<")
                    (backtrack (string-append "<" char)))))))
    (define (in-esm-part token)
      (let ((char (read-char src-port)))
        (case char
          (("%") (end-esm-part
                  (lambda (get-string)
                    (write get-string token)
                    (in-esm-part token))))
          (else (if (eof-object? char)
                    (error "bad esm")
                    (begin
                      (write-char char token)
                      (in-esm-part token)))))))
    (define (end-esm-part backtrack)
      (let ((char (read-char src-port)))
        (case char
          ((">")
           (set! next
                 (lambda ()
                   (text-part (open-output-string))))
           ("%>"))
          (("%")
           (let ((next-char (read-char src-port)))
             (if (eof-object? next-char)
                 (backtrack "%>")
                 (backtrack (string-append "%%" next-char)))))
          (else
           (if (eof-object? char)
               (backtrack "%>")
               (backtrack (string-append "%%" char)))))))
    (set! next
          (lambda ()
            (text-part (open-output-string))))
    (lambda ()
      (next))))

(define (test-lex src-port)
  (let ((lexer (make-lexer src-port)))
    (do ((token (lexer) (lexer)))
        ((eof-object? token) (print "end!!!"))
      (print "'" token "'"))))
      
(define (esm-compile src-port)
  (let ((_out (open-output-string "")))
    (define (text-part token)
      (case token
        (("<%") (start-scheme-part (get-token)))
        (("<%%") (start-display-scheme-part tokens))
        (("%>") (end-scheme-part tokens))
        (else (text-part tokens (string-append str token)))))))

