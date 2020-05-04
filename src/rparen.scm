(include "./red-paren.scm"
         "./red-paren.scm")

(import (scheme base) (scheme read) (scheme file) (scheme process-context)
        (scheme write) (color-paren red-paren)
        (color-paren red-paren default-rules))

(define (main-script)
  (let ((targets (cdr (command-line))))
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
