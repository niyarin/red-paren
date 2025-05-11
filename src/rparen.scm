(include "./red-paren.scm")

(import (scheme base) (scheme read) (scheme file) (scheme process-context)
        (scheme write) (color-paren red-paren)
        (srfi 13)
        (color-paren red-paren default-rules))

(define (display-about-red-paren)
  (write-string
    "Usage: rparen file-name ..."
    (current-error-port)))

(define (read-scm-code* input-port)
  (let loop ((input (read input-port))
             (res '()))
     (if (eof-object? input)
       (reverse res)
       (loop (read input-port) (cons input res)))))

(define (read-all input-port)
  (let loop ((input (read-line input-port))
             (res ""))
    (if (eof-object? input)
      res
      (loop (read-line input-port) (string-append res input  "\n")))))

(define (replace-str s from to)
  (let* ((found (string-contains s from))
         (found-right (and found (+ found (string-length from)))))
    (if found
      (string-append (substring s 0 found)
                     to
                     (replace-str (substring s found-right (string-length s))
                                  from
                                  to))
      s)))

(define (read-flisp-code input-port)
  (let* ((str (read-all input-port))
         (replaced (replace-str (replace-str str ",." ",@");;WIP
                                "#\\linefeed" "#\\newline")));;WIP
    (read-scm-code* (open-input-string replaced))))


(define (read-scm-code file-name)
  (call/cc (lambda (break)
    (with-exception-handler
      (lambda (err)
        (call-with-input-file
          file-name
          (lambda (input-port)
            (break (read-flisp-code input-port)))))
      (lambda ()
        (call-with-input-file
          file-name
          (lambda (input-port)
            (read-scm-code* input-port))))))))

(define (main-script)
  (let ((targets (cdr (command-line))))
    (when (null? targets)
      (display-about-red-paren))
    (for-each
      (lambda (file-name)
        (let ((code (read-scm-code file-name)))
              (red-paren/lint code
                              red-paren/default-rules
                              `(file-name ,file-name))))
      targets)))

(main-script)
