(define-library (color-paren red-paren default-rules)
   (import (scheme base) (scheme cxr))
   (export red-paren/default-rules)
   (begin
      (define %assoc-rules
        `(((assv quote) (assv (quote ,symbol?) x) (assq (quote ,symbol?) x))
          ((assoc quote) (assoc (quote ,symbol?) x) (assq (quote ,symbol?) x))))

      (define %eq-rules
        `(((eqv?) (eqv? (quote ,symbol?) x) (eq? (quote ,symbol? x)))
          ((equal?) (equal? (quote ,symbol?) x) (eq? (quote ,symbol? x)))))

      (define %let-rules
        `(((let) (let ((bind v)) bind)
                 v)))

      (define %ientity-rules
        `(((lambda) (lambda (x) x) values)))

      (define %control-rules
        `(((if) (if test true-expression) (when test true-expression))
          ((when not) (when (not test) expressions ...)
                      (unless test expressions ...))
          ((cond =>) (cond ptn1 ... (test => (lambda (x) x)) ptn2 ...)
                     (cond ptn1 ... (test) ptn2 ...))
          ((and) (and x ... (and  y ...) z ...)
                 (and x ... y ... z ...))
          ((or) (or a (or b ... ))
                (or a b ... ))
          ((if) (if v w (if x y z))
                (cond
                  (v w)
                  (x y)
                  (else z)))
          ((if) (if test true #f) (and test true))
          ((cond else)
            (cond head ...
                  (x exp1)
                  (y exp1)
                  z ...
                  (else else-exp ...))
            (cond head ...
                  ((or x y) exp1)
                   z ...
                   (else else-exp ...)))
          ((cond else)
           (cond head ...
                 (test exp1 ... )
                 (else exp1 ... ))
           (cond head ...
                 (else test exp1 ...)))))

      (define %arithmetic-rules
        `(((=) (= x 0) (zero? x))
          ((=) (= 0 x) (zero? x))
          ((zero? modulo) (zero? (modulo x 2)) (even? x))
          ((= modulo) (= (modulo x 2) 1) (odd? x))
          ((not zero? modulo) (not (zero? (modulo x 2))) (odd? x))
          ((>) (> x 0) (positive? x))
          ((<) (< x 0) (negative? x))))

      (define %pair-rules
        `(((car) (car (car x)) (caar x))
          ((cdr) (cdr (cdr x)) (cddr x))
          ((car cdr) (car (cdr x)) (cadr x))
          ((car cdr) (cdr (car x)) (cdar x))
          ((list) (list x x x) (make-list 3 x))
          ((list) (list x x x x) (make-list 4 x))
          ((list) (list x x x x x) (make-list 5 x))
          ((cddddr car) (car (cddddr x)) (list-ref x 4))
          ((cddr caddr) (caddr (cddr x)) (list-ref x 4))
          ((cddddr cadr) (cadr (cddddr x)) (list-ref x 5))
          ((cddddr caddr) (caddr (cddddr x)) (list-ref x 6))
          ((append list) (append (list x) ls) (cons x ls))
          ((append list) (append (list x ...) y) (cons* x ... y))
          ((append list) (append (list x) ls1 ls2 ...)
                         (cons x (append ls1 ls2 ...)))))

      (define %verbose-define-rules
        `(((define car) (define (foo x) (car x)) (define foo car))
          ((define cdr) (define (foo x) (cdr x)) (define foo cdr))
          ((define cadr) (define (foo x) (cadr x)) (define foo cadr))
          ((define caddr) (define (foo x) (caddr x)) (define foo caddr))))

      (define %scheme-list-rules
        `(((cons lambda) (lambda (a b) (cons b a)) xcons "" (scheme list))
          ((cons) (cons a (cons b c)) (cons* a b c) "" (scheme list))
          ((cons cons*) (cons* x ... (cons b c)) (cons* x ... b c) "" (scheme list))
          ((cons cons*) (cons* a b) (cons a b) "" (scheme list))
          ((values car cdr) (values (car x) (cdr x)) (car+cdr x) "" (scheme list))
          ((list car cadr) (list (car x) (cadr x)) (take x 2) "" (scheme list))
          ((list car cadr caddr) (list (car x) (cadr x) (caddr x)) (take x 3) "" (scheme list))
          ((list car cadr caddr cadddr) (list (car x) (cadr x) (caddr x) (cadddr x)) (take x 4) "" (scheme list))
          ((filter map lambda) (filter (lambda (x) x) (map f ls ...)) (filter-map f ls ...) "" (scheme list))
          ((filter lambda) (filter (lambda (x) (pred? x)) some-list) (filter pred? some-list))
          ((not pair?) (not (pair? x)) (not-pair? x) "" (scheme list))
          ((apply append map) (apply append (map f ls ...))
                              (append-map f ls ...)
                              "" (scheme list))
          ((apply append) (apply append list-of-list)
                          (concatenate list-of-list)
                          "" (scheme list))
          ((filter lambda not ) (filter (lambda (x) (not (pred x))) ls)
                                (remove pred ls)
                                "" (scheme list))
          ((append reverse) (append (reverse rev-head) tail) (append-reverse rev-head tail)
                            "" (scheme list))
          ((map list) (map list ls) (zip ls) "" (scheme list))))


      (define %call/cc-rules
        '(((call-with-current-continuation)
           (call-with-current-continuation x)
           (call/cc x))))

      (define %order-rules
        '(((read) (fn (read) (read))
                  (let* ((arg1 (read)) (arg2 (read))) (fn arg1 arg2))
                  "Execution order is unspecified(\"R7RS 4.1.3.  Procedure calls\")"
                  '())))

      (define %delay-rules
        '(((delay force) (delay (force expression))
                         (delay-force expression))))

      (define %cond->case-rules
        (let ((symbol1? (lambda (x) (and (symbol? x) 1)))
              (symbol2? (lambda (x) (and (symbol? x) 2)))
              (symbol3? (lambda (x) (and (symbol? x) 3))))
           `(((cond eq? quote else) (cond ((eq? x (quote ,symbol1?)) expression1)
                                          ((eq? x (quote ,symbol2?)) expression2)
                                          (else expression3))
                                    (case x
                                        ((,symbol1?) expression1)
                                        ((,symbol2?) expression2)
                                        (else expression3))))))

      (define %redundancy-bool
         '(((not eq?) (not (eq? x #f)) x)))

      (define red-paren/default-rules
        (append %assoc-rules
                %control-rules
                %let-rules
                %arithmetic-rules
                %pair-rules
                %scheme-list-rules
                %call/cc-rules
                %ientity-rules
                %order-rules
                %delay-rules
                %verbose-define-rules
                %redundancy-bool
                %cond->case-rules))))
