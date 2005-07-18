(define-module esm.gauche
  (require "esm/esm-gauche")
  (export define-esm
          esm-compile esm-result esm-run
          esm-result* esm-run*
          *esm-version* *esm-default-environment*))
(select-module esm.gauche)

(require "esm/esm-base")

(provide "esm/gauche")
