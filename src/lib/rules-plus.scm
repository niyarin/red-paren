(define-library (niyarin rules+)
   (import (scheme base)(scheme case-lambda) (scheme list) (scheme write))
   (export rules+/match rules+/expand rules+/match-expand)
   (begin
      (define (%alists-distinct . args)
        (let ((alist (concatenate args)))
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
             ((not-pair? rule)
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
             ((not-pair? input)
              (break #f))
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

      (define (%new-rule&duplicates-alist rule symbols duplicates)
        (let ((res-alist (map (lambda (x) (list x 1)) duplicates)))
          (letrec
            ((gen-sym
               (lambda (base-symbol)
                 (let loop ((cnt (cadr (assq base-symbol res-alist))))
                   (let ((new (string->symbol
                                (string-append (symbol->string base-symbol)
                                               (number->string cnt)))))
                     (if (not (memq new symbols))
                       new
                       (loop (+ cnt 1)))))))
             (loop (lambda (obj)
                       (cond
                         ((list? obj) (map loop obj))
                         ((vector? obj) (vector-map loop obj))
                         ((and (symbol? obj)
                               (memq obj duplicates)
                               (assq obj res-alist))
                          => (lambda (apair)
                               (let ((new-sym (gen-sym obj)))
                                 (begin
                                   (set-cdr! (cdr apair) (cons new-sym
                                                               (cddr apair)))
                                   (set-car! (cdr apair) (+ (cadr apair) 1))
                                   new-sym))))
                         (else obj)))))
            (values (loop rule)
                    (map (lambda (apair)
                           (cons (car apair)
                                 (cddr apair)))
                           res-alist)))))

      (define (%calc-duplicates ls literals ellipsis)
        (let loop ((ls ls)
                   (res '()))
          (cond
            ((null? ls) res)
            ((or (eq? (car ls) ellipsis)
                 (memq (car ls) literals)
                 (eq? (car ls) '_))
             (loop (cdr ls) res))
            ((memq (car ls) (cdr ls))
              (loop (remove (lambda (x) (eq? x (car ls))) ls)
                    (cons (car ls) res)))
            (else
              (loop (cdr ls) res)))))

      (define (%search-symbols obj excluded-list)
        (let* ((ltop (list #f))
               (tconc (cons ltop ltop)))
          (let loop ((obj obj))
            (cond
              ((pair? obj)
               (loop (car obj))
               (loop (cdr obj)))
              ((vector? obj)
               (vector-for-each loop obj))
              ((and (symbol? obj)
                    (not (memq obj excluded-list)))
               (set-cdr! (cdr tconc)
                         (list obj))
               (set-cdr! tconc
                         (cddr tconc)))
              (else)))
          (cdar tconc)))

      (define (%match-boot ellipsis literal rule input)
           (call/cc
             (lambda (break)
               (let* ((rule-symbols (%search-symbols rule (cons ellipsis literal)))
                      (duplicates-rules-symbols (%calc-duplicates rule-symbols literal ellipsis)))
                  (let-values (((new-rule duplicates-alist) (%new-rule&duplicates-alist rule rule-symbols duplicates-rules-symbols)))
                     (let* ((match-res (%match ellipsis literal new-rule '() input '() break))
                            (generated-symbols (append-map cdr duplicates-alist))
                            (not-duplicate-res (remove (lambda (apair)
                                                         (memq (car apair) generated-symbols))
                                                       match-res))
                            (dres
                                (map (lambda (sym-duplicate-sym-list)
                                       (let* ((dls-chk (map (lambda (sym)
                                                              (assq sym
                                                                    match-res))
                                                       (cdr sym-duplicate-sym-list)))
                                              (dls (if (find (lambda (x) x) dls-chk)
                                                     #f
                                                     (map cdr dls-chk))))
                                          (if (or (not dls)
                                                  (not (null? (remove (lambda (x) (equal? x (car dls))) dls))))
                                            #f
                                            (cons (car sym-duplicate-sym-list)
                                                  (car dls)))))
                                     duplicates-alist)))
                        (if (memq #f dres)
                          #f
                          (%alists-distinct
                                  not-duplicate-res
                                  dres
                                  (map (lambda (x) (list x)) rule-symbols)))))))))

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
                ((not-pair? ls)
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
