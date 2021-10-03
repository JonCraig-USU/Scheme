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

; COUNTING works by recursing back to 1 and appending all the way back
(define (countingNumbers limit)
  (if (> limit 1)
    (append (countingNumbers (- limit 1)) (list limit))
    (list limit)
  )
)

; EVEN recurses back to one checking the modulo of the number and 2 then appending back up
(define (evenNumbers limit)
  (if (> limit 1) 
    (if (= (mod limit 2) 0) 
      (append (evenNumbers (- limit 1)) (list limit))
      (evenNumbers(- limit 1))
    )
    ()
  )
)

; Prime numbers starts a count to the limit then runs a map to check for all primes
(define (primeNumbers limit)
  (if (> limit 2)
    (if (isP limit 2)
      (append (primeNumbers (- limit 1)) (list limit))
      (primeNumbers (- limit 1))
    )
    (list 2)
  )
)

(define (isP num div)
  (cond
    ((= (mod num div) 0)
      #f
    )
    ((> (* div div) num)
      #t
    )
    (else 
      (isP num (+ div 1))
    )
  )
)

; MERGE verifies that there are parts remaining in the list
; if there is check the firt values on each list and place the largest on the 'front'
; of the current list then recurse to the next set of list using the remaining parts
(define (merge listOne listTwo)
  (cond
    ((< (length listOne) 1)
      listTwo)
    ((< (length listTwo) 1)
      listOne)
    ((< (car listOne) (car listTwo))
      (append (list (car listOne)) (merge (cdr listOne) listTwo))
    )
    (else 
      (append (list (car listTwo)) (merge listOne (cdr listTwo)))
    )
  )
)


; WRAP passes the same list with the front moved to the back to the next level
; until the desired depth is reached and returns that answer
(define (wrap numberToWrap aList)
  (if (<= numberToWrap 0)
    aList
    (wrap (- numberToWrap 1) (append (cdr aList) (list (car aList))))
  )
)

; SUBLISTS uses the map feature to apply countingNumbers to everything
; in the lists creating a list of the sub lists
(define (subLists aList)
  (map (lambda (i) (countingNumbers i)) aList)
)

; REDUC works in 2 parts
; takes the list of a lists and passes to a map to seperate and prepare for the second function
; the 2nd functinon takes care of creating the actual functions and recursing through the sub lists
(define (reduc f init listOne)
  (if (<= (length listOne) 1)
    (f init (car listOne))
    (f (reduc f init (cdr listOne)) (car listOne))
  )
)

(define (reduceLists func initialValue listOfLists)
  (map (lambda (i) (reduc func initialValue i)) listOfLists)
)

