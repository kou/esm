#!/usr/bin/env gosh

(use file.util)
(use srfi-1)

(define *ignore-directories* (map x->string
                                  '(.svn CVS RCS)))

(define (main args)
  (pop! args)
  (install-directory "lib"
                     (gauche-site-library-directory)
                     (and (not (null? args))
                          (string=? (pop! args) "test")))
  0)

(define (install-directory from to test?)
  (directory-fold from
                  (lambda (file knil)
                    (let ((target (sys-dirname
                                   (string-scan file from 'after))))
                      (install-file file
                                    (string-append to target)
                                    test?)))
                  #t
                  :lister
                  (lambda (dir knil)
                    (let ((target (string-scan dir from 'after)))
                      (if (member (sys-basename target)
                                  *ignore-directories*
                                  string=?)
                          '()
                          (begin
                            (make-installed-directory
                             (string-join (list to target)
                                          "/")
                             test?)
                            (directory-list dir
                                            :children? #t
                                            :add-path? #t)))))))

(define (install-file file dir test?)
  (let ((target (string-join (list dir (sys-basename file))
                             "/")))
    (print #`"installing ,|file| => ,|target|")
    (if (not test?)
        (begin
          (copy-file file
                     target
                     :if-exists :supersede
                     :safe #t)
          (sys-chmod target #o644)))))

(define (make-installed-directory dir test?)
  (print #`"making installed directory ,|dir|")
  (if (not test?)
      (begin
        (make-directory* dir)
        (sys-chmod dir #o755))))
