#!/usr/bin/env gosh

(require "esm/esm-base")
;(use esm.gauche)

(define (main args)
  (esm-run (standard-input-port)
           (interaction-environment))
  0)
