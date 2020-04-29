(include "./lib/rules-plus.scm")
(include "./rules.scm")

(define-library (color-paren red-paren)
   (import (scheme base) (scheme cxr) (scheme write)
           (niyarin rules+) (color-paren red-paren default-rules))
   (export red-paren/lint)
   (begin
      (define *limit-recursion 10000)

      (define (%lint1 input rules)
        (let loop ((rules rules))
             (cond
               ((null? rules) #f)
               ((rules+/match-expand (caar rules) (cadar rules) (caddar rules) input)
                => (lambda (suggestion)
                     (let ((comment
                             (if (null? (cdddar rules))
                               "" (cadddr (car rules))))
                           (target-library
                             (if (or (null? (cdddar rules))
                                     (null? (cddddr (car rules))))
                               '() (cadr (cdddar rules)))))
                       (list suggestion comment target-library))))
               (else
                 (loop (cdr rules))))))

      (define (%assert from to comment lib)
        (display "----------")(newline)
        (unless (null? lib) (display lib)(display " : "))
        (display comment)(newline)(newline)
        (display "   ")(write from)(newline)
        (display "=> ")(write to)(newline)(newline))

      (define (%config-ref key config default)
        (cond
          ((assq key config)
           => cadr)
          (else default)))

      (define (%lint1-loop code rules)
         (let loop ((suggestion (list code))
                    (nest 0))
           (cond
             ((= nest *limit-recursion) suggestion)
             ((%lint1 (car suggestion) rules)
              => (lambda (_suggestion)
                   (loop _suggestion (+ nest 1))))
             ((> nest 0) suggestion)
             (else #f))))

      (define (%lint code rules execution-type res-box config)
          (let loop ((code code))
              (cond
                ((not (list? code)))
                ((eq? (car code) 'quote))
                ((%lint1-loop code rules)
                 => (lambda (suggestion)
                      (let ((suggested-code (car suggestion)))
                         (cond
                           ((eq? execution-type 'command-line)
                            (%assert code suggested-code
                                     (cadr suggestion) (caddr suggestion)))
                           ((eq? execution-type 'program)
                            (set-car! res-box
                                      (cons `(,code ,suggested-code)
                                            (car res-box))))
                           (else))
                         (for-each
                           (lambda (_code)
                             (loop _code))
                           code))))
                (else
                  (for-each
                    (lambda (_code)
                      (loop _code))
                    code)))))

      (define (red-paren/lint code rules . config)
        (let* ((output-type (%config-ref 'execution-type config 'command-line))
               (res-box (list '())))
           (%lint code rules output-type res-box config)
           (if (eq? output-type 'command-line)
             (car res-box))))))
