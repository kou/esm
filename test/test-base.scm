#!/usr/bin/env gosh

(use test.unit)
(use esm.gauche)

(define-test-case "test basic esm"
  ("test scheme code part"
   (assert-each (cut assert-equal "before after" <>)
                (list (esm-result* "<% (+ 1 2) %>before after")
                      (esm-result* "before<% %> after")
                      (esm-result* "before <% 1 %>after")
                      (esm-result* "before after<% 2 %>"))))
  ("test display code part"
   (assert-each (cut assert-equal "before medium after" <>)
                (list (esm-result* "before <%= \"medium\" %> after")
                      (esm-result* "before <%= 'medium %> after")
                      (esm-result* "before <%= (string #\\m #\\e #\\d
                                                       #\\i #\\u #\\m)
                                           %> after"))))
  ("test comment part"
   (assert-each (cut assert-equal "before after" <>)
                (list (esm-result* "<%; %>before after")
                      (esm-result* "before<%; a %> after")
                      (esm-result* "before <%; (( %>after")
                      (esm-result* "before after<%; ))
                                                    )) %>"))))
  ("test defined esm"
   (define-esm test-esm "test/test.esm")
   (assert-equal (string-incomplete->complete
                  (read-block 1000000
                              (open-input-file "test/test.esm.expected")))
                 (test-esm)))
  ("test nested esm"
   (define-esm parent "test/parent.esm")
   (define-esm child "test/child.esm")
   (assert-equal (string-incomplete->complete
                  (read-block 1000000
                              (open-input-file "test/parent.esm.expected")))
                 (parent)))
  )
