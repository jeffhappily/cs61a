; Q1
(define (compose-all funcs)
  (if (null? funcs)
      (lambda (x) x)
      (lambda (x) ((compose-all (cdr funcs)) ((car funcs) x))))
)

; Q2
(define (tail-replicate x n)
  (define (tail x n ls)
    (if (= n 0) 
      ls
      (tail x (- n 1) (cons x ls))))

  (tail x n '())
)
