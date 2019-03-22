(define (find s predicate)
  (cond 
   ((null? s) #f)
   ((predicate (car s)) (car s))
   (else (find (cdr-stream s) predicate))
  )
)

(define (scale-stream s k)
  (cond
   ((null? s) nil)
   (else (cons-stream (* (car s) k) 
                      (scale-stream (cdr-stream s) k)))
  )
)

(define (has-cycle s)
  (define (cycle s1 s2)
   (cond
    ((eq? s1 s2) #t)
    ((null? (cdr-stream s2)) #f)
    ((null? (cdr-stream (cdr-stream s2))) #f)
    (else (cycle (cdr-stream s1) (cdr-stream (cdr-stream s2))))
   )
  )
  (cond 
   ((null? s) #f)
   ((null? (cdr-stream s)) #f)
   (else (cycle s (cdr-stream s)))
  )

)
(define (has-cycle-constant s)
  (define (cycle s1 s2)
   (cond 
    ((eq? s1 s2) #t)
    ((null? (cdr-stream s2)) #f)
    ((null? (cdr-stream (cdr-stream s2))) #f)
    (else (cycle (cdr-stream s1) (cdr-stream (cdr-stream s2))))
   )
  )
  (cond
   ((null? s) #f)
   ((null? (cdr-stream s)) #f)
   (else (cycle s (cdr-stream s)))
  )
)
