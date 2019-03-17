(define (accumulate combiner start n term)
  (if (= n 0)
      start
      (combiner (term n) (accumulate combiner start (- n 1) term))
  )
)

(define (accumulate-tail combiner start n term)
  (define (acc n res)
    (if (= n 0)
        res
        (acc (- n 1) (combiner res (term n)))
        )
  )
  (acc n start)
)

(define-macro (list-of expr for var in seq if filter-fn)
  `(map (lambda (,var) ,expr)
        (filter (lambda (,var) ,filter-fn) ,seq))
)
