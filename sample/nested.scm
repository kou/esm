#!/usr/bin/env gosh

(use esm.gauche)

(define (main args)
  (define-esm parent "parent.esm" (current-module))
  (define-esm child "child.esm" (current-module) arg)
  (display (parent))
  0)
