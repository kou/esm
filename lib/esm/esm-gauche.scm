(select-module esm.gauche)

;; For make-lexer
(define-macro (esm-make-output)
  "(open-output-string)")

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
  `(,backtrack ,get-string))

(define-macro (set-action! . actions)
  `(set! action (lambda () ,@actions)))

(define-macro (init-action)
  '(define action #f))

(define-macro (do-action)
  '(action))

;; For esm-compile
(define-macro (make-result)
  '(open-output-string))

(define-macro (write-to-result value result)
  `(display ,value ,result))

(define-macro (result->string result)
  `(get-output-string ,result))

(define (esm-output target output result target-output-proc)
  (display " (display " result)
  (target-output-proc target result)
  (display " " result)
  (display output result)
  (display ") " result))

(define (esm-output-text target output result)
  (esm-output target output result write))

(define (esm-output-scheme target output result)
  (esm-output target output result display))

(define-macro (esm-get-output output)
  `(string-append "(get-output-string " ,output ")"))

