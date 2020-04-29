(define-library (color-paren red-paren default-rules)
   (import (scheme base))
   (export red-paren/default-rules)
   (begin
      (define %assoc-rules
        `(((assv) (assv (quote ,symbol?) x) (assq (quote ,symbol?) x))
          ((assoc) (assoc (quote ,symbol?) x) (assq (quote ,symbol?) x))))

      (define %control-rules
        `(((if) (if test true-expression) (when test true-expression))
          ((when not) (when (not test) expressions ...) (unless test expressions ...))))

      (define %arithmetic-rules
        `(((=) (= x 0) (zero? x))
          ((=) (= 0 x) (zero? x))))

      (define red-paren/default-rules
        (append %assoc-rules
                %control-rules
                %arithmetic-rules))))
