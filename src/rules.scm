(define-library (color-paren red-paren default-rules)
   (import (scheme base))
   (export red-paren/default-rules)
   (begin
      (define %assoc-rules
        `(((assv) (assv (quote ,symbol?) x) (assq (quote ,symbol?) x))
          ((assoc) (assoc (quote ,symbol?) x) (assq (quote ,symbol?) x))))

      (define %control-rules
        `(((if) (if test true-expression) (when test true-expression))
          ((when not) (when (not test) expressions ...)
                      (unless test expressions ...))))

      (define %arithmetic-rules
        `(((=) (= x 0) (zero? x))
          ((=) (= 0 x) (zero? x))
          ((=) (zero? (modulo x 2)) (even? x))
          ((=) (= (modulo x 2) 1) (odd? x))
          ((=) (not (zero? (modulo x 2))) (odd? x))
          ((=) (> x 0) (positive? x))))

      (define %pair-rules
        `(((car) (car (car x)) (caar x))
          ((cdr) (cdr (cdr x)) (cddr x))
          ((car cdr) (car (cdr x)) (cadr x))
          ((car cdr) (cdr (car x)) (cdar x))))

      (define %scheme-list-rules
        `(((apply append map) (apply append (map f ls ...))
                              (append-map f ls ...)
                              "" (scheme list))
          ((apply append) (apply append list-of-list)
                          (concatenate list-of-list)
                          "" (scheme list))
          ((filter lambda not ) (filter (lambda (x) (not (pred x)) body) ls)
                                (remove pred ls)
                                "" (scheme list))))

      (define red-paren/default-rules
        (append %assoc-rules
                %control-rules
                %arithmetic-rules
                %pair-rules
                %scheme-list-rules))))
