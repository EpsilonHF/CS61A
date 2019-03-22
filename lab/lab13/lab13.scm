; Q1
(define (compose-all funcs)
  (lambda (x)
   (if (null? funcs) x
    ((compose-all (cdr funcs)) ((car funcs) x))
   )
  )
)

; Q2
(define (tail-replicate x n)
  (define (rep x n lst)
   (cond
    ((= n 0) lst)
    (else (rep x (- n 1) (cons x lst)))))
  (rep x n nil)
)
