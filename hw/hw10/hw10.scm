(define (accumulate combiner start n term)
  (if (= n 0)
      start
	  (accumulate combiner (combiner start (term n)) (- n 1) term)
  )
)

(define (accumulate-tail combiner start n term)
  (if (= n 0)
      start
	  (accumulate combiner (combiner start (term n)) (- n 1) term)))

(define-macro (list-of expr for var in seq if filter-fn)
	`(map (lambda (,var) ,expr) (filter (lambda (,var) ,filter-fn) ,seq))
)
