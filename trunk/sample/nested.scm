#!/usr/bin/env gosh

(use esm.gauche)

(define-esm parent "parent.esm")
(define-esm child "child.esm")

(define (main args)
  (display (parent))
  0)
