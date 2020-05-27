(define-library (niyarin loose-rules)
   (import (scheme base)(scheme list)(scheme case-lambda) (scheme write))
   (export loose-rules/match loose-rules/expand loose-rules/match-expand)
   (begin
      (define (%parse-list ellipsis-symbol ls)
        (let loop ((ls ls)
                   (type 'list)
                   (head '())
                   (tail '())
                   (ellipsis #f))
          (cond
            ((null? ls)
             `((type ,type)
               (head ,(reverse head))
               (tail ,(reverse tail))
               (last-cdr '())
               (ellipsis ,ellipsis)))
            ((not (pair? ls))
             `((type ,(if (eq? type 'list)
                        'improper-list
                        'improper-ellipsis-list))
               (head ,(reverse head))
               (tail ,(reverse tail))
               (last-cdr ,ls)
               (ellipsis ,ellipsis)))
            ((and (pair? (cdr ls))
                  (eq? (cadr ls) ellipsis-symbol))
             (loop (cddr ls)
                   'ellipsis
                   head
                   tail
                   (car ls)))
            ((eq? type 'list)
             (loop (cdr ls)
                   type
                   (cons (car ls) head)
                   tail
                   ellipsis))
            (else
              (loop (cdr ls)
                    type
                    head
                    (cons (car ls) tail)
                    ellipsis)))))

      (define (proper-list-last-cdr-pair improper-list)
        (let ((plist-box (list #f)))
          (let loop ((ls improper-list)
                     (cell plist-box))
            (cond
              ((not (pair? ls))
               (cons (cdr plist-box) ls))
              (else
                (set-cdr! cell (list (car ls)))
                (loop (cdr ls)
                      (cdr cell)))))))

      (define (%append-res res1 res2)
        (append
         (map (lambda (apair2)
                (let* ((k (car apair2))
                       (v (cdr apair2))
                       (apair1 (assq k res1)))
                  (if apair1
                    (cons k (append (cdr apair1) (list v)))
                    (cons k (list v)))))
              res2)
         res1))

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

      (define (%remove-ellipsis-type type)
        (case type
           ((improper-ellipsis-list) 'improper-list)
           ((ellipsis) 'list)
           (else type)))

      (define (%match-list ellipsis-symbol literals rule input-list break)
        (let* ((_input (proper-list-last-cdr-pair input-list))
               (input-last-cdr (cdr _input))
               (parsed-list (%parse-list ellipsis-symbol rule))
               (type (cadr (assq 'type parsed-list)))
               (head (cadr (assq 'head parsed-list)))
               (tail (cadr (assq 'tail parsed-list)))
               (last-cdr (cadr (assq 'last-cdr parsed-list)))
               (ellipsis (cadr (assq 'ellipsis parsed-list))))

           (let loop ((type type)
                      (input (car _input))
                      (head head)
                      (res '()))
             (cond
               ((not (null? head))
                (loop type
                      (cdr input)
                      (cdr head)
                      (append (%match ellipsis-symbol literals (car head) (car input) break) res)))
              ((and (null? head)
                    (or (eq? type 'ellipsis) (eq? type  'improper-ellipsis-list)))
               (let ((loop-cnt (- (length input) (length tail))))
                 (when (< loop-cnt 0) (break #f))
                 (let i-loop ((i loop-cnt)
                              (input input)
                              (res res))
                   (cond
                     ((zero? i) (loop (%remove-ellipsis-type type) input tail res))
                     (else
                       (i-loop (- i 1)
                               (cdr input)
                               (%alists-distinct (%append-res res
                                                              (%match ellipsis-symbol
                                                                      literals
                                                                      ellipsis
                                                                      (car input)
                                                                      break)))))))))
              ((and (not (null? input-last-cdr ))
                    (memq type '(improper-list improper-ellipsis-list)))
               (append
                 (%match ellipsis-symbol literals last-cdr input-last-cdr break)
                 res))
              ((eq? type 'improper-list)
               (append
                 (%match ellipsis-symbol literals last-cdr input break)
                 res))
              ((and (eq? type 'list) (not (null? input)))
                (break #f))
              (else res)))))


      (define (%match ellipsis literals rule input break)
        (cond
          ((and (pair? rule) (pair? input))
           (%match-list ellipsis literals rule input break))
          ((pair? rule) (break #f))
          ((and (vector? rule) (vector? input))
           (%match-list ellipsis literals (vector->list rule) (vector->list input) break))
          ((vector? rule) (break #f))
          ((and (symbol? rule)
                (not (memq rule literals)))
           (list (cons rule input)))
          ((equal? rule input) '())
          (else (break #f))))

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

      (define (%match-boot ellipsis literal rule input)
        (call/cc
          (lambda (break)
            (let* ((rule-symbols (%search-symbols rule (cons ellipsis literal)))
                   (duplicates-rules-symbols (%calc-duplicates rule-symbols literal ellipsis)))
               (let-values (((new-rule duplicates-alist) (%new-rule&duplicates-alist rule rule-symbols duplicates-rules-symbols)))
                  (let* ((match-res (%match ellipsis literal new-rule input break))
                         (generated-symbols (append-map cdr duplicates-alist))
                         (not-duplicate-res (remove (lambda (apair)
                                                      (memq (car apair) generated-symbols))
                                                    match-res))
                         (dres
                             (map (lambda (apair)
                                    (let ((dls (map (lambda (sym)
                                                      (cdr (assq sym match-res)))
                                                    (cdr apair))))
                                       (if (not (null? (remove (lambda (x) (equal? x (car dls))) dls)))
                                         #f
                                         (cons (car apair) (car dls)))))
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
          ((and (symbol? template)
                (assq template alist))
           => (lambda (apair)
                (%tree-ref (cdr apair) (reverse refs) break template)))
          (else template)))

      (define (%expand-boot ellipsis template alist)
          (call/cc
            (lambda (break)
                (%expand ellipsis template alist '() break))))

      (define loose-rules/match
        (case-lambda
          ((rule input)
           (%match-boot '... '() rule input))
          ((literals rule input)
           (%match-boot '... literals rule input))
          ((ellipsis literals rule input)
           (%match-boot ellipsis literals rule input))))

      (define loose-rules/expand
        (case-lambda
          ((template alist)
           (%expand-boot '... template alist))
          ((ellipsis template alist)
           (%expand-boot ellipsis template alist))))

      (define (%match-expand-boot ellipsis literal rule template input)
        (let ((match-res (loose-rules/match ellipsis literal rule input)))
          (if match-res
            (loose-rules/expand ellipsis template match-res)
            #f)))

      (define loose-rules/match-expand
        (case-lambda
          ((ellipsis literals rule template input)
           (%match-expand-boot ellipsis literals rule template input))
          ((literals rule template input)
           (%match-expand-boot '... literals rule template input))
          ((rule template input)
           (%match-expand-boot '... '() rule template input))))))
