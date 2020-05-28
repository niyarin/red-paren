(include "./loose-rules.scm")

(define-library (color-paren red-paren macro-util)
   (import (scheme base) (scheme cxr) (scheme list) (niyarin loose-rules))
   (export red-paren-macro-util/expand1
           red-paren-macro-util/macr-list->rev-macro-list
           red-paren-macro-util/multi-expand1
           red-paren-macro-util/multi-expand)
   (begin
      (define (red-paren-macro-util/expand1 code macro-list)
        (let loop ((mls macro-list))
          (cond
            ((null? mls) #f)
            ((not (pair? code)) #f)
            ((eq? (caar mls) (car code))
               (let* ((rule-template (car mls))
                      (literal (cadr rule-template))
                      (rule (caddr  rule-template))
                      (template (cadddr rule-template)))
                  (cond
                    ((loose-rules/match-expand literal rule template code))
                    ((loop (cdr mls))))))
            (else (loop (cdr mls))))))

      (define (red-paren-macro-util/multi-expand1 code macro-list)
        (let loop ((mls macro-list)
                   (res '()))
           (cond
             ((null? mls) res)
             ((not (pair? code)) res)
             ((eq? (caar mls) (car code))
               (let* ((rule-template (car mls))
                      (literal (cadr rule-template))
                      (rule (caddr  rule-template))
                      (template (cadddr rule-template)))
                  (cond
                    ((loose-rules/match-expand literal rule template code)
                     => (lambda (expanded)
                          (loop (cdr mls) (cons expanded res))))
                    (else (loop (cdr mls) res)))))
             (else (loop (cdr mls) res)))))

      (define (%uniq-reverse-list ls)
        (let loop ((ls ls)
                   (res '()))
          (if (null? ls)
            res
            (loop (remove (lambda (x) (equal? (car ls) x))
                          (cdr ls))
                  (cons (car ls) res)))))

      (define (red-paren-macro-util/multi-expand code macro-list nest)
        (let loop ((i 0)
                   (res (list code)))
          (if (< i nest)
            (loop (+ i 1)
                  (%uniq-reverse-list
                        (append-map (lambda (code) (cons code (red-paren-macro-util/multi-expand1 code macro-list)))
                                    res)))
            res)))

      (define (red-paren-macro-util/macr-list->rev-macro-list macro-list)
         (map (lambda (ls)
                (let* ((org-rule (caddr ls))
                       (rule (cons (car ls) (cdr org-rule))))
                   (list (car ls) (cadr ls) (cadddr ls) rule)))
              macro-list))))
