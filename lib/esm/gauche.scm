(define-module esm.gauche
  (require "esm/gauche-macro")
  (require "esm/esm-base")
  (export <esm> result run src-of
          esm-compile esm-result esm-run define-esm))
(select-module esm.gauche)

(define *esm-default-environment* (interaction-environment))

(define-class <esm> ()
  ((src :accessor src-of :init-keyword :src))
  )
   
(define-method initialize ((self <esm>) initargs)
  (next-method)
  (slot-set! self 'src
             (esm-compile (src-of self))))

(define-method result ((self <esm>) . env)
  (apply esm-eval (src-of self) env))

(define-method run ((self <esm>) . env)
  (display (apply result self env)))

(provide "esm/gauche")
