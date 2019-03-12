(define (caar x) (car (car x))) (define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (if (null? rests) rests
    (cons 
      (append (list first) (car rests)) 
      (cons-all first (cdr rests)))))

(define (zip pairs)
  (define (iter pairs)
	(if (null? pairs) '(())
		(cons-all
		  (caar pairs)
		  (iter (cdr pairs)))))
  (cond 
    ((null? pairs) '(() ()))
    ((null? (car pairs)) '())
    (else
	  (append
		(iter pairs)
		(zip (map (lambda (x) (cdr x)) pairs))))))
;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (rec i s)
    (if (null? s) s
        (cons (cons i (cons (car s) nil)) (rec (+ i 1) (cdr s)))))
  (rec 0 s))
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((or (null? denoms) (< total 0)) '())
    ((= total 0) '(()))
    (else 
     (append
       (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
       (list-change total (cdr denoms))))))
  
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
		 expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
		 expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
		   (cons form
			 (cons params (map let-to-lambda body)))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
		   (let ((args (zip values)))
			 (cons 
			   (cons 'lambda 
				(cons (car args) 
				(map let-to-lambda body))) 
			   (map let-to-lambda (cadr args)))) 
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
		 (map
		   let-to-lambda expr)
         ; END PROBLEM 19
         )))

(define (test expr)
  (zip expr))
