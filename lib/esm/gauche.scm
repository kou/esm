(define-module esm.gauche
  (require "esm/esm-gauche")
  (require "esm/esm-base")
  (export define-esm
          esm-compile esm-result esm-run
          esm-result* esm-run*
          *esm-version* *esm-default-environment*))
(select-module esm.gauche)

(provide "esm/gauche")
