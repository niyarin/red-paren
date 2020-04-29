# red-paren
Small Scheme Linter

The output of the number of lines and the output with the original shape are not yet implemented.

## Usage

```bash

gosh rparen.scm xxxxx.scm yyyyyy.scm ...

```

## Examples


```
>gosh src/rparen.scm example-resources/badcode1.scm

----------


   (if (not (and (> x 0) (= (modulo (+ x y) 2) 0))) (display (car (car y))))
=> (unless (and (> x 0) (= (modulo (+ x y) 2) 0)) (display (car (car y))))

----------


   (> x 0)
=> (positive? x)

----------


   (= (modulo (+ x y) 2) 0)
=> (even? (+ x y))

----------


   (car (car y))
=> (caar y)
```
