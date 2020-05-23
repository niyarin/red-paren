(define-library (color-paren red-paren default-rules)
   (import (scheme base))
   (export red-paren/default-rules)
   (begin
      (define %assoc-rules
        `(((assv quote) (assv (quote ,symbol?) x) (assq (quote ,symbol?) x))
          ((assoc quote) (assoc (quote ,symbol?) x) (assq (quote ,symbol?) x))))

      (define %eq-rules
        `(((eqv?) (eqv? (quote ,symbol?) x) (eq? (quote ,symbol? x)))
          ((equal?) (equal? (quote ,symbol?) x) (eq? (quote ,symbol? x)))))

      (define %control-rules
        `(((if) (if test true-expression) (when test true-expression))
          ((when not) (when (not test) expressions ...)
                      (unless test expressions ...))
          ((cond =>) (cond ptn1 ... (test => (lambda (x) x)) ptn2 ...)
                     (cond ptn1 ... (test) ptn2 ...))))

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
          ((append list) (append (list x) ls) (cons x ls))
          ((append list) (append (list x) ls1 ls2 ...)
                         (cons x (append ls1 ls2 ...)))))

      (define %scheme-list-rules
        `(((cons lambda) (lambda (a b) (cons b a)) xcons "" (scheme list))
          ((cons) (cons a (cons b c)) (cons* a b c) "" (scheme list))
          ((cons cons*) (cons* x ... (cons b c)) (cons* x ... b c) "" (scheme list))
          ((values car cdr) (values (car x) (cdr x)) (car+cdr x) "" (scheme list))
          ((list car cadr) (list (car x) (cadr x)) (take 2 x) "" (scheme list))
          ((list car cadr caddr) (list (car x) (cadr x) (caddr x)) (take 3 x) "" (scheme list))
          ((list car cadr caddr cadddr) (list (car x) (cadr x) (caddr x) (cadddr x)) (take 4 x) "" (scheme list))
          ((filter map lambda) (filter (lambda (x) x) (map f ls ...)) (filter-map f ls ...) "" (scheme list))
          ((apply map list) (apply map list ls ...) (zip ls ...) "" (scheme list))
          ((not pair?) (not (pair? x)) (not-pair? x) "" (scheme list))
          ((apply append map) (apply append (map f ls ...))
                              (append-map f ls ...)
                              "" (scheme list))
          ((apply append) (apply append list-of-list)
                          (concatenate list-of-list)
                          "" (scheme list))
          ((filter lambda not ) (filter (lambda (x) (not (pred x)) body) ls)
                                (remove pred ls)
                                "" (scheme list))
          ((append reverse) (append (reverse rev-head) tail) (append-reverse rev-head tail)
                            "" (scheme list))))


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

      (define red-paren/default-rules
        (append %assoc-rules
                %control-rules
                %arithmetic-rules
                %pair-rules
                %scheme-list-rules
                %call/cc-rules
                %order-rules
                %delay-rules))))
