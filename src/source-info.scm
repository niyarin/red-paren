(define-library (color-paren red-paren source-info)
   (export red-paren/source-info)
   (cond-expand
     (gauche
         (import (gauche base))
         (begin (define (red-paren/source-info obj)
                    (debug-source-info obj))))
     (else
       (import (scheme base))
       (begin (define (red-paren/source-info obj) #f)))))
