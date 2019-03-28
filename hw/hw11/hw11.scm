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
  ; tmp stores list of references to every element looped through
  ; think of it like a pointers
  ; 
  (define (has-cycle-rec s tmp)
	(cond 
      ((null? s) #f)
      ((contains tmp s) #t)
      (else (has-cycle-rec (cdr-stream s) (cons s tmp)))))

  (has-cycle-rec s nil)
)

(define (contains lst s)
  (cond
    ((null? lst) #f)
    ((eq? (car lst) s) #t)
    (else (contains (cdr lst) s))))


(define (has-cycle-constant s)
  (define (has-cycle-rec s sr)
    (cond
      ((or
         (null? sr)
         (null? (cdr-stream sr))) #f)
      ((or 
         (eq? s sr)
         (eq? s (cdr-stream sr))) #t)
      (else (has-cycle-rec (cdr-stream s) (cdr-stream (cdr-stream sr))))))
  (has-cycle-rec s (cdr-stream s)))
