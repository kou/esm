#!/usr/local/bin/gosh
;; -*- scheme -*-

(add-load-path "/home/kou/work/gauche/esm/lib/")

(define (main args)
  (cgi-main
   (lambda (param)
     (require "bbs")
     `(,(cgi-header)
       ,(let ((action (cgi-get-parameter "action" param)))
          (cond ((equal? action "submit")
                 (if (submit
                      (cgi-get-parameter "name" param)
                      (cgi-get-parameter "title" param)
                      (cgi-get-parameter "body" param))
                     (add-message "submit success")
                     (add-message "submit failed"))))
          (body (cgi-get-parameter "max" param :default 10)))))
   :on-error (lambda (e)
               `(,(cgi-header)
                 ,(show-error e)))))
               
