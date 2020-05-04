# red-paren
Small Scheme Linter

## Usage

```bash

gosh rparen.scm xxxxx.scm yyyyyy.scm ...

```

## Examples


```
>gosh src/rparen.scm example-resources/badcode1.scm


----------
example-resources/badcode1.scm 1


   (if (not (and (> x 0) (= (modulo (+ x y) 2) 0))) (display (car (car y))))
=> (unless (and (> x 0) (= (modulo (+ x y) 2) 0)) (display (car (car y))))

----------
example-resources/badcode1.scm 1


   (> x 0)
=> (positive? x)

----------
example-resources/badcode1.scm 1


   (= (modulo (+ x y) 2) 0)
=> (even? (+ x y))

----------
example-resources/badcode1.scm 2


   (car (car y))
=> (caar y)
```
