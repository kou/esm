#!/usr/bin/env gosh

(use esm.gauche)

(define (main args)
  (let ((src (if (null? (cdr args))
                 (standard-input-port)
                 (open-input-file (cadr args)))))
    (esm-run src (current-module))
    0))
