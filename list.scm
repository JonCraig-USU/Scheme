; The gcd function computes the greatest common divisor of m and n.
; Inputs - m First number
;          n Second number
; Output - the GCD
; Examples
;    (gcd 4 2) returns 2
;    (gcd 7 3) returns 1
(define (gcd m n)
  (cond ((< m n) (gcd m (- n m)))
        ((< n m) (gcd (- m n) n))
        (else m)))

;; check appendages and verify base case
(define (countingNumbers limit)
  (cond (<= limit 1) (1)
    (else (append (countingNumbers (- limit 1) limit)))
  )
)

;; check that numbers are actually appending onto it
;; note they are most likely not at the moment
(define (evenNumbers limit)
  (if (> limit 1) 
    (cond (= (modulo limit 2) 0) 
      (evenNumbers (- limit 1) limit)
      (else (evenNumbers(- limit 1)))  
    )
  )
)

;; use a lambda function and recurse down to 0 appending on way back, or not
(define (primeNumbers limit)
)

;; check sub list creation and usage
(define (merge listOne listTwo)
  (cond (>= (car listOne) (car listTwo)) 
    append ((list (car listOne)) (merge (cdr listOne) listTwo))
    (else (append(car listTwo) (merge listOne (cdr listTwo)))
    )
  )
)


(define (wrap numberToWrap aList)
  (cond (>= numberToWrap 1) 
    (wrap (- numberToWrap 1) (append (list (cdr aList)) (list (car aList))))
    (else (aList))
  )
)

(define (subLists aList)
  (cond (null? ))

  ;; list (count (cdr aList) aList)
)

;; this willl need a few things
;; base case is to recurse down to initial value
;; recurse may have to go down to 0, check my other functions allow
;; function recurse ans current val
(define (reduceLists func initialValue listOfLists)
)

(define (swap myList)
   (if (>= (length myList) 2)
      (append (list (cadr myList)) (list (car myList)) (cddr myList))
      myList
   )
)
