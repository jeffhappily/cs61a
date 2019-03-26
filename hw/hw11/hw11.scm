(define (find s predicate)
  (cond
	((null? s) #f)
	((predicate (car s)) (car s))
	(else (find (cdr-stream s) predicate)))
)

(define (scale-stream s k)
  (if (null? s) '()
	  (cons-stream (* k (car s)) (scale-stream (cdr-stream s) k)))
)

(define (has-cycle s)
  (define (has-cycle-rec s sr)
	(if (null? sr) #f
		(or
		  (eq? (car s) (car sr))
		  (has-cycle-rec s (cdr-stream sr))
		  (has-cycle-rec (cdr-stream sr) (cdr-stream sr)))))

  (has-cycle-rec s (cdr-stream s))
)
(define (has-cycle-constant s)
  'YOUR-CODE-HERE
)
