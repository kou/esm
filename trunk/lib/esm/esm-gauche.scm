(select-module esm.gauche)

;; For esm-result
(define *esm-default-environment* (interaction-environment))

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
  `(lambda (get-string unput-string)
     (if (and get-string
              (not (string=? get-string "")))
         (write-to-token get-string ,token))
     (if (and unput-string
              (not (string=? unput-string "")))
         (reader 'unput unput-string))
     (,next-action ,token)))

(define-macro (do-backtrack backtrack get-string unput-string)
  `(,backtrack ,get-string ,unput-string))

;; For esm-compile
(define-macro (make-result)
  '(open-output-string))

(define-macro (write-to-result value result)
  `(display ,value ,result))

(define-macro (result->string result)
  `(get-output-string ,result))

(define-macro (esm-output-start result)
  `(write-to-result " (display " ,result))

(define-macro (esm-output-end output result)
  `(begin
     (write-to-result ,output ,result)
     (write-to-result ")" ,result)))

(define (esm-output target output result target-output-proc)
  (esm-output-start result)
  (esm-output-contents target result target-output-proc)
  (esm-output-end output result))

(define (esm-output-contents target result target-output-proc)
  (target-output-proc target result)
  (write-to-result " " result))

(define (esm-output-text-contents target result)
  (esm-output-contents target result write))

(define (esm-output-scheme-contents target result)
  (esm-output-contents target result display))

(define (esm-output-text target output result)
  (esm-output target output result write))

(define (esm-output-scheme target output result)
  (esm-output target output result display))

(define-macro (esm-get-output output)
  `(string-append "(get-output-string " ,output ")"))
