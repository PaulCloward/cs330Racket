;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists-lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(check-temps1 temps) → boolean
; temps : (listof number)
;Consumes a list of temperature measures and checks whether all measurements are between 5 and 95 degrees celsius (inclusively.)
(define (check-temps1 temps)
  (if (empty? temps)
      true
      (if (> (first temps) 95)
          false
          (if (< (first temps) 5)
              false
              (check-temps1 (rest temps))))))

;create a bunch of lists
(define aList (list 23))

(define bList (list 5 95 23 43 34 54))

(define cList (list 34 23 43 54 4))

(define dList (list 45 96 54 65))

;Check-temps1 empty test
(check-expect (check-temps1 empty) true)

;Check-temps1 test: all correct temperature tests
(check-expect (check-temps1 aList) true)

;Check-temps1 test: all correct temperature tests, 2 temps at the inside boundary
(check-expect (check-temps1 bList) true)

;Check-temps1 test:  incorrect temperature tests. One temp right outside the boundary
(check-expect (check-temps1 cList) false)

;Check-temps1 test:  incorrect temperature tests. One temp right outside the boundary
(check-expect (check-temps1 dList) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(check-temps temps low high) → boolean
;  temps : (listof number)
;  low : number
;  high : number
;Consumes a list of temperature measures and checks whether all measurements are between low and high degrees celsius (inclusively.)
(define (check-temps temps low high)
  (if (empty? temps)
      true
      (if (> (first temps) high)
          false
          (if (< (first temps) low)
              false
              (check-temps1 (rest temps))))))

;create a bunch of lists
(define aaList (list 23))

(define bbList (list 5 95 23 43 34 54))

(define ccList (list 34 23 43 54 4))

(define ddList (list 45 96 54 65))

;Test the empty list
(check-expect (check-temps empty 0 100) true)

;list should between 1 and 100. True test
(check-expect (check-temps aaList 1 100) true)

;test of a low of 5 and high of 54. Boundary test returning true
(check-expect (check-temps bbList 5 95) true)

;boundary testing. The low should fail because the list has a 4. And the low is 5
(check-expect (check-temps ccList 5 95) false)

;boundary testing. The high should fail because the list has a 96.
(check-expect (check-temps ddList 5 96) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(convert digits) → number
;  digits : (listof number)
;Consumes a list of digits (numbers between 0 and 9) and produces the corresponding number. The first digit is the least significant, and so on.

(define (convert digits)
  (if (empty? digits)
      0
      (+ (first digits) (* (convert (rest digits)) 10))
      ))

;Empty convert digits test
(check-expect (convert empty) 0)

;First convert digits test
(check-expect (convert (list 2)) 2)

;Second convert digits test
(check-expect (convert (list 4 5 3 2 1)) 12354)

;Third convert digits test
(check-expect (convert (list 0 1 0 1 0)) 01010)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum a)
  (if (empty? a)
      0
      (+ (first a) (sum (rest a)))
      ))

;(average-price prices) → number
; prices : (listof number)
;Consumes a list of toy prices and computes the average price of a toy. The average is total of all prices divided by the number of toys.
(define (average-price prices)
  (if (empty? prices)
      0
      (/ (sum prices)(length prices))))

(check-expect (average-price empty) 0)
(check-expect (average-price (list 2)) 2)
(check-expect (average-price (list 4 4 4)) 4)
(check-expect (average-price (list 4)) 4)
(check-expect (average-price (list 9 9 9)) 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(convertFC fahrenheit) → (listof number?)
; fahrenheit : (listof number?)
;Converts a list of of Fahrenheit measurements to a list of Celsius measurements.
(define (convertFC fahrenheit)
  (if (empty? fahrenheit)
      empty
      (cons (* (- (first fahrenheit) 32) 5/9) (convertFC (rest fahrenheit)))
      )
  )

(check-expect (convertFC empty) empty)

;Testing -40 and 0 farenheit being converted 
(check-expect (convertFC (list 32)) (list 0))

;Testing 86 and 68 farenheit being converted
(check-expect (convertFC (list 86 68)) (list 30 20))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(eliminate-exp ua lotp) → (listof number)
; ua : number
; lotp : (listof number)
;Eliminates from lotp all toys whose price is greater than ua.
(define (eliminate-exp ua lotp)
  (if (empty? lotp)
      empty
      (if (<= (first lotp) ua)
          (cons (first lotp) (eliminate-exp ua (rest lotp)))
          (eliminate-exp ua (rest lotp)))
      ))
;test for empty list
(check-expect (eliminate-exp 5 empty) empty)

;test one for eliminate-exp
(check-expect (eliminate-exp 7 (list 3)) (list 3))

;test two for eliminate-exp
(check-expect (eliminate-exp 24 (list 24 24 45 543453 1 2 34 54 32 89 17 16 16 17)) (list 24 24 1 2 17 16 16 17))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(suffixes l) → (listof list?)
;  l : list?
; Produces a list of a suffixes of l
(define (suffixes l)
  (if(empty? l)
     empty
     (cons l (suffixes (rest l))))
  )
(check-expect (suffixes empty) empty)

;1st test for the suffixes procedure
(check-expect (suffixes (list 1)) (cons (cons 1 '()) '()))

;2nd test for the suffixes procedure
(check-expect (suffixes (list 3 2 3 2)) (cons (cons 3 (cons 2 (cons 3 (cons 2 '())))) (cons (cons 2 (cons 3 (cons 2 '()))) (cons (cons 3 (cons 2 '())) (cons (cons 2 '()) '())))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Represents an unknown ancestor
(define-struct unknown ())

;(struct person (name birthyear eyecolor father mother))
;  name : string
;  birthyear : number
;  eyecolor : symbol
;  father : (or/c unknown person)
;  mother : (or/c unknown person)
;Represents a person
(define-struct person (name birthyear eyecolor father mother))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(count-persons ftree) → number
;  ftree : (or/c unknown person)
; Counts the number of people in a family tree
(define (count-persons ftree)
  (if (unknown? ftree)
      0
      (+ (count-persons (person-mother ftree))(count-persons (person-father ftree)) 1)
      ))

(define u (make-unknown))
(define fred(make-person "fred" 1950 "green" u u))

;test case for unknown person
(check-expect (count-persons u) 0)

;test case with someone with no known parents
(check-expect (count-persons fred) 1)

(define wilma(make-person "wilma" 1950 "white" u u))
(define randy(make-person "randy" 1970 "blue" fred wilma))

(define kirk(make-person "kirk" 1943 "brown" u u))
(define paula(make-person "paula" 1945 "blue" u u))
(define shannon(make-person "shannon" 1971 "brown" kirk paula))

(define paul(make-person "paul" 1991 "hazel" randy shannon))

;test case of a normal family tree
(check-expect (count-persons paul) 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;total-person function
(define (total-person ftree)
  (if (unknown? ftree)
      0
      (+ (total-person (person-mother ftree))(total-person (person-father ftree)) (- 2016 (person-birthyear ftree)))
      ))



;(average-age ftree) → number?
;  ftree : (or/c unknown? person?)
;Return the average age of all the people in the family tree
(define (average-age ftree)
  (if (unknown? ftree)
      0
      (/ (total-person ftree) (count-persons ftree))
      ))

;Test unknown case
(check-expect (average-age u) 0)

;Test 1 of average-age: one person with unknown parents
(check-expect (average-age fred) 66)

;Test 2 of average-age: one person with 2 parents. The parents have unknown parents
(check-expect (average-age shannon) 63)

;Test 3 of average-age: one person with 2 parents. The parents have unknown parents
(check-within (average-age randy) 59.3 .1)

;Test 4 of  average age: Test all of Pauls family tree
(check-expect (average-age paul) 56)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(eye-colors ftree) → (listof symbol?)
;  ftree : (or/c unknown? person?)
;Produces a list of all eye colors in family tree. (An eye color may occur more than once in the list.)
(define (eye-colors ftree)
  (if (unknown? ftree)
      empty
      (append (eye-colors (person-mother ftree))
              (eye-colors (person-father ftree))
              (cons (person-eyecolor ftree) empty))))

;Test case unknown
(check-expect (eye-colors u) '())

;Test case 1: One single person without known parents
(check-expect (eye-colors fred) (cons "green" '()))

;Test case 2: All of Pauls family trees eyes
(check-expect (eye-colors paul) (cons "blue" (cons "brown" (cons "brown" (cons "white" (cons "green" (cons "blue" (cons "hazel" '()))))))))