(use esm.gauche)
(use file.util)
(use util.list)
(use www.cgi)
(use text.html-lite)
(use text.tree)

(define *message-list* '())
(define *data-directory* "data")

(define (add-message message)
  (push! *message-list* message))

(define (get-messages)
  *message-list*)

(define-esm* header "header.esm")
(define-esm* footer "footer.esm")
(define-esm* body "body.esm" n)
(define-esm* form "form.esm")
(define-esm* show-list "list.esm" n)
(define-esm* show-error "error.esm" e)

(define h html-escape-string)

(define (datafiles)
  (directory-list
   *data-directory*
   :add-path? #f
   :children? #t
   :filter (lambda (x) (rxmatch #/^\d+$/ x))))

(define (new-datafile)
  (number->string
   (+ 1
      (apply max
             0 ; fall back on
             (map string->number
                  (datafiles))))))

(define (empty? str)
  (rxmatch #/^\s*$/ str))

(define (submit name title body)
  (if (not (or (empty? name)
               (empty? title)
               (empty? body)))
      (let ((number (new-datafile)))
        (call-with-output-file (string-append *data-directory* "/" number)
          (lambda (out)
            (if out
                (write
                 `(("number" . ,number)
                   ("name" . ,name)
                   ("title" . ,title)
                   ("body" . ,body))
                 out)
                #f))))
      #f))

(define (read-files n)
  (read-items
   (take* (sort (map string->number
                     (datafiles))
                >)
          n)))

(define (read-items files)
  (map (lambda (file)
         (call-with-input-file (tree->string (list
                                              *data-directory*
                                              "/"
                                              (number->string file)))
           read))
       files))

(define (item-number item)
  (cdr (assoc "number" item)))
(define (item-name item)
  (cdr (assoc "name" item)))
(define (item-title item)
  (cdr (assoc "title" item)))
(define (item-body item)
  (cdr (assoc "body" item)))

