#!/usr/bin/env gosh

(use esm.gauche)

(define (main args)
  (run (make <esm>
         :src (standard-input-port))
       (interaction-environment))
  0)
