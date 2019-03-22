; Q4
(define (rle s)
  (define (helper s ele num)
   (cond
    ((null? s) (cons-stream (list ele num) nil))
    ((= ele (car s)) (helper (cdr-stream s) ele (+ num 1)))
    (else (cons-stream (list ele num) (rle s)))
   )
  )
  (cond
   ((null? s) nil)
   (else (helper (cdr-stream s) (car s) 1))
  )
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
  (define (helper n pre poster)
   (cond
    ((null? poster) (append pre (cons n nil)))
    ((> n (car poster)) (helper n (append pre (cons (car poster) nil)) (cdr poster)))
    (else (append (append pre (cons n nil)) poster))
   )
  )
  (helper n '() s)

)

; Q6
(define (deep-map fn s)
  (cond
   ((null? s) nil)
   ((integer? s) (fn s))
   ((list? s) (cons (deep-map fn (car s)) (deep-map fn (cdr s))))
  )
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
  (cond
   ((null? s) nil)
   (else (cons (car s) (filter (lambda (x) (not (eq? (car s) x))) (unique (cdr s)))))
  )
)

(define (count name s)
  (cond
   ((null? s) 0)
   ((eq? (car s) name) (+ 1 (count name (cdr s))))
   (else (count name (cdr s)))
  )
)

(define (tally names)
  (map (lambda (x) (cons x (count x names))) (unique names))
)
