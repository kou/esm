(define-module esm.gauche
  (require "esm/esm-base")
  (export <esm> esm-compile esm-result esm-run))
(select-module esm.gauche)

(define-class <esm> ()
  ((src :accessor src-of :init-keyword :src))
  )
   
(define-method initialize ((self <esm>) initargs)
  (next-method)
  (slot-set! self 'src
             (esm-compile (src-of self))))

(define-method result ((self <esm>) args)
  (next-method)
  (eval (src-of self)
        (get-keyword :env args
                     (interaction-environment))))

(define-method run ((self <esm>) args)
  (next-method)
  (display (apply run self args)))

(provide "esm/gauche")
