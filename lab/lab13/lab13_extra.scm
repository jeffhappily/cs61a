; Q4
(define (rle s)
  (define (rec lst a n)
    (cond
      ((null? lst) (cons-stream (list a n) '()))
      ((= (car lst) a) (rec (cdr-stream lst) a (+ n 1)))
      (else (cons-stream (list a n) (rec (cdr-stream lst) (car lst) 1)))))

  (if (null? s) '() (rec (cdr-stream s) (car s) 1))
)

; Q4 testing functions
(define (list-to-stream lst)
    (if (null? lst) nil
                    (cons-stream (car lst) (list-to-stream (cdr lst))))
)

(define (stream-to-list s)
    (if (null? s) nil
                 (cons (car s) (stream-to-list (cdr-stream s))))
)

; Q5
(define (insert n s)
  (define (rec n lst head)
    (cond
      ((null? lst) (append head (list n)))
      ((> (car lst) n) (append head (cons n lst)))
      (else (rec n (cdr lst) (append head (list (car lst)))))))

  (rec n s '())
)

; Q6
(define (deep-map fn s)
  (cond
    ((null? s) '())
    ((list? (car s)) (cons (deep-map fn (car s)) (deep-map fn (cdr s))))
    (else (cons (fn (car s)) (deep-map fn (cdr s)))))
)

; Q7
; Feel free to use these helper procedures in your solution
(define (map fn s)
  (if (null? s) nil
      (cons (fn (car s))
            (map fn (cdr s)))))

(define (filter fn s)
  (cond ((null? s) nil)
        ((fn (car s)) (cons (car s)
                            (filter fn (cdr s))))
        (else (filter fn (cdr s)))))

; Implementing and using these helper procedures is optional. You are allowed
; to delete them.
(define (unique s)
  'YOUR-CODE-HERE
  nil
)

(define (count name s)
  'YOUR-CODE-HERE
  nil
)

(define (tally names)
  'YOUR-CODE-HERE
  nil
)
