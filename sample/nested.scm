#!/usr/bin/env gosh

(use esm.gauche)

(define-esm parent "parent.esm" (current-module))
(define-esm child "child.esm" (current-module) arg)

(define (main args)
  (display (parent))
  0)
