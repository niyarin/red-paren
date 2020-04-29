(include "./lib/rules-plus.scm")
(include "./rules.scm")

(define-library (color-paren red-paren)
   (import (scheme base) (scheme cxr) (scheme write)
           (niyarin rules+) (color-paren red-paren default-rules))
   (export red-paren/lint)
   (begin
      (define (%lint1 input rules)
        (let loop ((rules rules))
             (cond
               ((null? rules) #f)
               ((rules+/match-expand (caar rules) (cadar rules) (caddar rules) input)
                => (lambda (assert)
                     assert))
               (else
                 (loop (cdr rules))))))

      (define (%assert from to)
        (display "----------")(newline)(newline)
        (display "   ")(write from)(newline)
        (display "=> ")(write to)(newline)(newline))

      (define (%config-ref key config default)
        (cond
          ((assq key config)
           => cadr)
          (else default)))

      (define (%lint code rules execution-type res-box config)
          (let loop ((code code))
              (cond
                ((not (list? code)))
                ((eq? (car code) 'quote))
                ((%lint1 code rules)
                 => (lambda (suggested-code)
                      (begin
                         (cond
                           ((eq? execution-type 'command-line) (%assert code suggested-code))
                           ((eq? execution-type 'program)
                            (set-car! res-box
                                      (cons `(,code ,suggested-code)
                                            (car res-box))))
                           (else))
                         (for-each
                           (lambda (_code)
                             (loop _code)
                             suggested-code)
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
