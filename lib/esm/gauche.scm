(define-module esm.gauche
  (require "esm/esm-gauche")
  (require "esm/esm-base")
  (export define-esm define-esm*
          esm-compile esm-result esm-run
          esm-result* esm-run*
          *esm-version* *esm-default-environment*))
(select-module esm.gauche)

(define *esm-default-environment* (interaction-environment))

(define-macro (define-esm* name filename . args)
  `(define-esm ,name ,filename (current-module) ,@args))

(provide "esm/gauche")
