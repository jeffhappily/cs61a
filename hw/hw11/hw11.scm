(define (find s predicate)
  (cond
	((null? predicate) #f)
	((predicate (car s)) #t)
	(else (find s (cdr-stream s))
)

(define (scale-stream s k)
  'YOUR-CODE-HERE
)

(define (has-cycle s)
  'YOUR-CODE-HERE
)
(define (has-cycle-constant s)
  'YOUR-CODE-HERE
)
