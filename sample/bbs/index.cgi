#!/usr/local/bin/gosh
;; -*- scheme -*-

(use esm.gauche)
(use www.cgi)

(define-esm* header "header.esm")
(define-esm* footer "footer.esm")
(define-esm* top "top.esm")
(define-esm* edit "edit.esm")

(define (main args)
  (cgi-main
   (lambda (param)
     (let (action (cgi-get-parameter "action" param))
       (cond ((equal? action "edit")
              (edit))
             ((equal? action "submit")
              (top))
             (else
              (top)))))))
