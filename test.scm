#!/usr/bin/env gosh

(add-load-path ".")

; (use gauche.vm.debugger)
; (enable-debug)

(use esm)

(define (main args)
  (let ((result (esm-compile (standard-input-port))))
    (print result)
    (let ((port (open-input-string result)))
      (let loop ((next (read port)))
        (if (eof-object? next)
            #f
            (begin
              (eval next (scheme-report-environment 5))
              (loop (read port))))))))
