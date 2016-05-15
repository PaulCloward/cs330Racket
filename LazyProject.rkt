#lang lazy

(define print-only-errors #f)
(define (test l r)
  (if (equal? l r)
      (if print-only-errors
          ()
          (printf "Test Passed~n"))
      (printf "Test Failed.~nActual:   ~S ~nExpected: ~S~n" l r)))

;(take-while p l) → (listof any/c)
;  p : (any/c . -> . boolean)
;  l : (listof any/c)
; Returns the prefix of l such that for all elements p returns true.
(define (take-while p l)
  (if (empty? l)
      empty
      (if (p (first l))
          (cons (first l) (take-while p (rest l)))
          empty)))

(test (take-while odd? (list 1 3 4))
      (list 1 3))
(test (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2))
      (list 1 2 3 4))

(define infinite-list (cons 0 (map (lambda (x) (+ x 1)) infinite-list)))

;(build-infinite-list f) → (listof any/c)
;  f : (exact-nonnegative-integer? . -> . any/c)
;Lazily constructs the infinite list such that (list-ref (build-infinite-list f) i) returns (f i).
(define (build-infinite-list f)
  (map f infinite-list))

;buildiing-infinite-list tests
(test (list-ref (build-infinite-list (lambda (x) (* x 2))) 3) 6)
(test (list-ref (build-infinite-list (lambda (x) (/ x 2))) 6) 3)
(test (list-ref (build-infinite-list (lambda (x) (+ x 1))) 0) 1)


(define (prime? n)
  (if (or (= n 0)(= n 1))
      false
  (not-divisible-by-smaller n (floor (sqrt n)))))

(define (not-divisible-by-smaller n d)
    (if (= d 1)
        true
        (if (= (remainder n d) 0)
          false
          (not-divisible-by-smaller n (- d 1)))))

(test (prime? 2) true)
(test (prime? 5) true)
(test (prime? 3) true)
(test (prime? 7) true)
(test (prime? 71) true)
(test (prime? 101) true)
(test (prime? 4) false)
(test (prime? 100) false)
(test (prime? 9) false)
(test (prime? 1) false)
(test (prime? 0) false)






