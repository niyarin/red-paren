(include "./red-paren.scm"
         "./red-paren.scm");TODO:fix same libname

(import (scheme base) (scheme read) (scheme file) (scheme process-context)
        (scheme write) (color-paren red-paren)
        (color-paren red-paren default-rules))

(define (display-about-red-paren)
  (write-string
    "Usage: rparen file-name ..."
    (current-error-port)))

(define (main-script)
  (let ((targets (cdr (command-line))))
    (when (null? targets)
      (display-about-red-paren))
    (for-each
      (lambda (file-name)
        (call-with-input-file
          file-name
          (lambda (input-port)
            (let ((code (let loop ((input (read input-port))
                                   (res '()))
                           (if (eof-object? input)
                             (reverse res)
                             (loop (read input-port) (cons input res))))))
              (red-paren/lint code
                              red-paren/default-rules
                              `(file-name ,file-name))))))
      targets)))

(main-script)
