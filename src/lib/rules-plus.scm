(define-library (niyarin rules+)
   (import (scheme base)(scheme case-lambda))
   (export rules+/match rules+/expand rules+/match-expand)
   (begin
      (define (%alists-distinct . args)
        (let ((alist (apply append args)))
          (let loop ((als alist)
                     (res '())
                     (keys '()))
            (cond
              ((null? als) res)
              ((memq (caar als) keys)
               (loop (cdr als) res keys))
              (else
                (loop (cdr als)
                      (cons (car als) res)
                      (cons (caar als) keys)))))))

      (define (%ellipsis-rule? rule ellipsis)
        (and (pair? rule)
             (pair? (cdr rule))
             (eq? (cadr rule) ellipsis)))

      (define (%match-list ellipsis literals rule input res nests break)
         (unless (pair? input)(break #f))
         (let loop ((rule rule)
                    (input input)
                    (break break)
                    (nest-cnt 0)
                    (res res))
           (cond
             ((and (null? rule) (null? input)) res)
             ((null? rule) (break  #f))
             ((and (%ellipsis-rule? rule ellipsis)
                   (null? (cddr rule))
                   (null? input))
              res)
             ((null? input) (break #f))
             ((not (pair? rule))
              (%match ellipsis literals rule nests input res break))
             ((%ellipsis-rule? rule ellipsis)
              (or (call/cc (lambda (_break)
                             (let ((new-res
                                     (%match ellipsis
                                             literals
                                             (car rule)
                                             (cons nest-cnt nests)
                                             (car input)
                                              res
                                              _break)))
                               (loop rule (cdr input) _break (+ nest-cnt 1) new-res))))
                  (loop (cddr rule) input break 0 res)))
             (else (loop (cdr rule)
                         (cdr input)
                         break
                         nest-cnt
                         (%match ellipsis literals (car rule)
                                 nests (car input) res break))))))

      (define (%tree-push-obj rev-nests input tree)
         (if (null? rev-nests)
           input
           (let loop ((nests (reverse rev-nests))
                      (tree tree))
             (cond
               ((null? (cdr nests))
                (append tree (list input)))
               ((= (length tree) (car nests))
                (append tree (list (loop (cdr nests) '()))))
               (else
                 (let ((copy (list-copy tree)))
                  (list-set!
                    copy
                    (- (length copy) 1)
                    (loop (cdr nests) (list-ref tree (- (length tree) 1))))
                  copy))))))

      (define (%match ellipsis literals rule nests input res break)
        (cond
          ((pair? rule) (%match-list ellipsis literals rule input res nests break))
          ((and (vector? rule) (vector? input))
           (%match-list ellipsis
                        literals
                        (vector->list rule)
                        (vector->list input)
                        res
                        nests
                        break))
          ((and (symbol? rule)
                (not (memq rule literals)))
           (let ((tree (cond ((assq rule res) => cdr) (else '()))))
            (cons (cons rule (%tree-push-obj nests input tree))
                  res)))
          ((procedure? rule)
           (if (rule input)
             (let ((tree (cond ((assq rule res) => cdr) (else '()))))
                (cons (cons rule (%tree-push-obj nests input tree))
                      res))
             (break #f)))
          ((equal? rule input) res)
          (else (break #f))))

      (define (%match-boot ellipsis literals rule input)
        (call/cc (lambda (break)
                   (%alists-distinct
                     (%match ellipsis literals rule '() input '() break)))))

      (define (%tree-ref tree refs break . debug-info)
         (let loop ((refs refs)
                    (tree tree))
           (cond
             ((null? refs) tree)
             ((not (list?  tree) )(error "Wrong depth to expand." debug-info))
             ((<= (length tree) (car refs))
              (break (length refs)))
             (else
               (loop (cdr refs) (list-ref tree (car refs)))))))

      (define (%expand-pair ellipsis template alist refs break)
        (let ((res-cell-top (list #f)))
            (let loop ((ls template)
                       (res-cell res-cell-top))
              (cond
                ((not (pair? ls))
                 (set-cdr! res-cell (%expand ellipsis ls alist refs break))
                 (cdr res-cell-top))
                ((and (pair? (cdr ls))
                      (eq? (cadr ls) ellipsis))
                    (let _loop ((i 0)(res-cell res-cell))
                      (let ((ok (call/cc (lambda (_break)
                                           (list (%expand ellipsis (car ls) alist (cons i refs) _break))))))
                        (cond
                          ((not (integer? ok))
                            (set-cdr! res-cell ok)
                            (_loop (+ i 1) (cdr res-cell)))
                          ((zero? (- ok 1))
                           (loop (cddr ls) res-cell))
                          (else
                            (break (- ok 1)))))))
                (else
                  (set-cdr! res-cell (list (%expand ellipsis (car ls) alist refs break)))
                  (loop (cdr ls) (cdr res-cell)))))))


      (define (%expand ellipsis template alist refs break)
        (cond
          ((pair? template)
           (%expand-pair ellipsis template alist refs break))
          ((and (or (symbol? template) (procedure? template))
                (assq template alist))
           => (lambda (apair)
                (%tree-ref (cdr apair) (reverse refs) break template)))
          (else template)))


      (define (%expand-boot ellipsis template alist)
          (call/cc
            (lambda (break)
                (%expand ellipsis template alist '() break))))

      (define rules+/match
        (case-lambda
          ((rule input)
           (%match-boot '... '() rule input))
          ((literals rule input)
           (%match-boot '... literals rule input))
          ((ellipsis literals rule input)
           (%match-boot ellipsis literals rule input))))

      (define rules+/expand
        (case-lambda
          ((template alist)
           (%expand-boot '... template alist))
          ((ellipsis template alist)
           (%expand-boot ellipsis template alist))))

      (define (%match-expand-boot ellipsis literal rule template input)
        (let ((match-res (rules+/match ellipsis literal rule input)))
          (if match-res
            (rules+/expand ellipsis template match-res)
            #f)))

      (define rules+/match-expand
        (case-lambda
          ((ellipsis literals rule template input)
           (%match-expand-boot ellipsis literals rule template input))
          ((literals rule template input)
           (%match-expand-boot '... literals rule template input))
          ((rule template input)
           (%match-expand-boot '... '() rule template input))))))
