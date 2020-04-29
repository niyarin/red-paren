(if (not (and (> x 0) (= (modulo (+ x y) 2) 0)))
  (display (car (car y))))
