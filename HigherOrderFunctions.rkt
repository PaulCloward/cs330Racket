;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HigherOrderFunctions-Lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (check-temps-filter temps)
 (filter check temps)  
  )

(define (check x)
  (if(<= x 95)
     (if(>= x 5)
      false
      true)
  true
  ))

;(check-temps1 temps) → boolean
; temps : (listof number) 
;Consumes a list of temperature measures and checks whether all measurements are between 5 and 95 degrees celsius (inclusively.)
(define (check-temps1 temps)
  (if (empty? (check-temps-filter temps))
      true
      false))

;Check-temps1: test 1. Only one in the list
(check-expect (check-temps1 (list 35)) true)
;Check-temps1: test 2. Boundary Case 1
(check-expect (check-temps1 (list 5 95)) true)
;Check-temps1: test 3. Boundary Case 2
(check-expect (check-temps1 (list 96 4)) false)
;Check-temps1: test 4. Bigger list
(check-expect (check-temps1 (list 45 63 23 12 32 88 88 5 95 74 32)) true)
;Check-temps1: test 5. Bigger list
(check-expect (check-temps1 (list 34 54 65 45 23 102 23 88 67)) false)
;Check-temps1: test 6. Empty Test
(check-expect (check-temps1 empty) true)
;----------------------------------------------------------------------------------------------------------------------------------------------------------------



(define (check-temps-highfilter temps high)
 (filter (lambda (x) (> x high))  temps)
  )

(define (check-temps-lowfilter temps low)
 (filter (lambda (x) (< x low))  temps)
  )

;(check-temps temps low high) → boolean
; temps : (listof number) 
; low : number 
; high : number
;Consumes a list of temperature measures and checks whether all measurements are between low and high degrees celsius (inclusively.)
(define (check-temps temps low high)
  (if (empty? (check-temps-highfilter temps high))
      (if (empty? (check-temps-lowfilter temps low))
         true
         false)
      false))

;Check-temps: test 1. Only one in the list
(check-expect (check-temps (list 35) 34 36) true)
;Check-temps: test 2. Boundary Case 1
(check-expect (check-temps (list 5 95) 5 95) true)
;Check-temps: test 3. Boundary Case 2
(check-expect (check-temps (list 96 4) 5 95) false)
;Check-temps: test 4. Bigger list
(check-expect (check-temps (list 45 63 23 30 32 68 68 23 71 74 32) 22 75) true)
;Check-temps: test 5. Bigger list
(check-expect (check-temps (list 34 54 65 45 23 102 23 88 67) 5 95) false)
;Check-temps: test 6. Empty List
(check-expect (check-temps empty 4 6) true)
;----------------------------------------------------------------------------------------------------------------------------------------------------------------

;(convert digits) → number
;digits : (listof number) 
;Consumes a list of digits (numbers between 0 and 9) and produces the corresponding number. The first digit is the least significant, and so on.
(define (convert digits)
  (foldr (lambda (x y) (+ x (* y 10))) 0 digits)
  )

;convert test case- case 1: 1 member in the list
(check-expect (convert (list 4)) 4)
;convert test case- case 2: many more members in the list
(check-expect (convert (list 4 3 2 1)) 1234)
;convert test case- case 3: many more members in the list
(check-expect (convert (list 1 2 3)) 321)
;convert test case- case 4: empty list
(check-expect (convert empty) 0)


;------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (sum-list prices)
(foldr + 0 prices)
  )

;(average-price prices) → number
; prices : (listof number) 
; Consumes a list of toy prices and computes the average price of a toy. The average is total of all prices divided by the number of toys
(define (average-price prices)
  (if (empty? prices)
      0
  (/ (sum-list prices) (length prices))))

;average-price test case- case 1: 1 member in the list
(check-expect (average-price (list 109)) 109)
;average-price test case- case 2: many more members in the list
(check-expect (average-price (list 1 2 3 4 5 6 7 8 9 10)) 5.5)
;average-price test case- case 3: 4 members in the list
(check-expect (average-price (list 1 2 3 4)) 2.5)
;averare-price test case- case 4: empty list
(check-expect (average-price empty) 0)


;------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (minus32 x)
  (- x 32))

(define (timesFunction x)
  (* x (/ 5 9)))

(define (filterMinus32 list)
 (map minus32 list))

(define (filterMulti list)
  (map timesFunction list))


;(convertFC fahrenheit) → (listof number)
; fahrenheit : (listof number) 
;Converts a list of of Fahrenheit measurements to a list of Celsius measurements
(define (convertFC fahrenheit)
  (filterMulti (filterMinus32 fahrenheit)))

;convertFC test case- case 1: 1 member in the list
(check-expect (convertFC (list 32)) (list 0))
;convertFC test case- case 2: many members in the list
(check-expect (convertFC (list 5 32 77)) (list -15 0 25))
;convertFC test case- case 3: many members in the list
(check-expect (convertFC (list 14 23 32 41 50)) (list -10 -5 0 5 10))
;convertFC test case- case 4: empty list
(check-expect (convertFC empty) empty)
;-----------------------------------------------------------------------------------------------------------------------------------------------------------------

;(eliminate-exp ua lotp) → (listof number)
;ua : number 
;lotp : (listof number) 
;Eliminates from lotp all toys whose price is greater than ua.
(define (eliminate-exp ua lotp)
  (filter (lambda (x) (<= x ua)) lotp))

;eliminate-exp test case- case 1: 1 member in the list
(check-expect (eliminate-exp 10 (list 9)) (list 9))
;eliminate-exp test case- case 2: many members in the list
(check-expect (eliminate-exp 7 (list 1 34 3 5 2 1 7 8)) (list 1 3 5 2 1 7))
;eliminate-exp test case- case 3: many members in the list
(check-expect (eliminate-exp 15.42 (list 1 34 15.40 15.43 5 100 32 8)) (list 1 15.40 5 8))
;eliminate-exp test case- case 4: empty list
(check-expect (eliminate-exp 7 empty) empty)
;----------------------------------------------------------------------------------------------------------------------------------------------------------------

;(compose-func after before) → (alpha . -> . gamma)
;  after : (beta . -> . gamma)
;  before : (alpha . -> . beta)
; Returns the composition of before and after.
(define (compose-func after before)
  (lambda (x) (after (before x))))

;Define functions to compose
(define (inc x) (+ x 1))
(define (square x) (* x x))

;Test compose-func
(check-expect ((compose-func inc square) 3) 10)
(check-expect ((compose-func square inc) 4) 25)

;------------------------------------------------------------------------------------------------------------------------------------------------------------------

;(flatten lolon) → (listof number)
; lolon : (listof (listof number))
;Produces a list of all the numbers in the elements of lolon
(define (flatten lolon)
 (if (empty? lolon)
     empty
     (append (first lolon)
             (flatten (rest lolon)))))
;flatten test case- case 1: 1 member in the list
(check-expect (flatten (list(list 1))) (list 1))
;flatten test case- case 2: many lists in the list
(check-expect (flatten (list(list 1 2 3 4) (list 5 6 7 8))) (list 1 2 3 4 5 6 7 8))
;flatten test case- case 3: many lists in the list
(check-expect (flatten (list(list 1 2 3 4) (list 15 16 17 18) (list 32 23 43 5))) (list 1 2 3 4 15 16 17 18 32 23 43 5))
;flatten test case- case 4: empty list
(check-expect (flatten empty) empty)

;----------------------------------------------------------------------------------------------------------------------------------------------------------------

;(flatten-foldr lolon) → (listof number)
; lolon : (listof (listof number))
;Produces a list of all the numbers in the elements of lolon
(define (flatten-foldr lolon)
  (foldr append '() lolon)
  )

;flatten-folder test case- case 1: 1 member in the list
(check-expect (flatten-foldr (list(list 1))) (list 1))
;flatten-folder test case- case 2: many lists in the list
(check-expect (flatten-foldr (list(list 1 2 3 4) (list 5 6 7 8))) (list 1 2 3 4 5 6 7 8))
;flatten-folder test case- case 3: many lists in the list
(check-expect (flatten-foldr (list(list 1 2 3 4) (list 15 16 17 18) (list 32 23 43 5))) (list 1 2 3 4 15 16 17 18 32 23 43 5))
;flatten-folder test case- case 4: empty list
(check-expect (flatten-foldr empty) empty)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------

;(bucket lon) → (listof (listof number?))
; lon : (listof number?) 
; Returns a list of sublists of adjacent equal numbers.
(define (bucket lon)
  (foldr (lambda (x y) (if (empty? y)
                           (cons (cons x empty) y)
                            (if (= x (first (first y)))
                                (cons (cons x (first y)) (rest y))
                                (cons (cons x empty) y))))
        empty lon))
;bucket test case- case 1: Only one member in list
(check-expect (bucket (list 3)) (list (list 3)))
;bucket test case- case 2: Many members in the list
(check-expect (bucket (list 1 1 2 2)) (list (list 1 1) (list 2 2)))
;bucket test case- case 3: Many members in the list
(check-expect (bucket (list 1 1 2 2 3 3 1 1 1 4 4 4 4 1)) (list (list 1 1) (list 2 2) (list 3 3) (list 1 1 1) (list 4 4 4 4) (list 1)))
;bucket test case- case 4: empty list
(check-expect (bucket empty) empty)
;bucket test case - case 5: doesn't combine, cons test case
(check-expect (bucket (list 1 2 3 4 5 6 7 8)) (list (list 1)(list 2)(list 3)(list 4)(list 5)(list 6)(list 7)(list 8)))

;---------------------------------------------------------------------------------------------------------------------------------
;(struct unknown ())
;Represents an unknown ancestor.
(define-struct unknown())

;(struct person (name birthyear eyecolor father mother))
;  name : string
;  birthyear : number
;  eyecolor : symbol
;  father : (or/c unknown person)
;  mother : (or/c unknown person)
;Represents a person.
(define-struct person (name birthyear eyecolor father mother)) 

;(tree-map f tree) → (or/c unknown person)
;      f : (string . -> . string)
;      tree : (or/c unknown person)
;Returns a tree where f has been applied to every person’s name in tree.
(define (tree-map f tree)
  (if (unknown? tree)
      (make-unknown)
      (make-person (f (person-name tree))
                   (person-birthyear tree)
                   (person-eyecolor tree)
                   (tree-map f (person-father tree))
                   (tree-map f (person-mother tree)))))

;Define persons
(define paula (make-person "Paula" 1950 "brown" (make-unknown) (make-unknown)))
(define kirk (make-person "Kirk" 1950 "brown" (make-unknown)(make-unknown)))
(define jean (make-person "Jean" 1950 "brown" (make-unknown) (make-unknown)))
(define bert (make-person "Bert" 1950 "brown" (make-unknown) (make-unknown)))

(define shannon (make-person "Shannon" 1970 "brown" kirk paula))
(define randy (make-person "Randy" 1970 "blue" bert jean))

(define paul (make-person "Paul" 1991 "hazel" randy shannon))
(define nobody(make-unknown))

;tree-map test case: Case 1- unknown person
(check-expect (tree-map (lambda (x) (string-append x " " "C")) nobody) (make-unknown))
;tree-map test case: Case 2- 1 person
(check-expect (tree-map (lambda (x) (string-append x " " "C")) bert) (make-person "Bert C" 1950 "brown" (make-unknown) (make-unknown)))
;tree-map test case: Case 3- 3 people
(check-expect (tree-map (lambda (x) (string-append x " " "C")) randy) (make-person "Randy C" 1970 "blue"
              (make-person "Bert C" 1950 "brown" (make-unknown) (make-unknown))
              (make-person "Jean C" 1950 "brown" (make-unknown) (make-unknown))))

;---------------------------------------------------------------------------------------------------------------------------------------------------------------


;(add-last-name tree lname) → (or/c unknown person)
;      tree : (or/c unknown person)
;      lname : string
;Returns a tree where lname has been appended to every person’s name in tree.
;You should use tree-map and string-append. You will not receive credit you do not re-use your tree-map.
(define (add-last-name tree lname)
  (tree-map (lambda (x) (string-append x " " lname)) tree))

;add-last-name Tests 
(check-expect (add-last-name nobody " ") (make-unknown))
(check-expect (add-last-name bert "Cloward") (make-person "Bert Cloward" 1950 "brown" (make-unknown) (make-unknown)))
(check-expect (add-last-name paul "Cloward")
              (make-person "Paul Cloward" 1991 "hazel"
              (make-person "Randy Cloward" 1970 "blue"
              (make-person "Bert Cloward" 1950 "brown" (make-unknown) (make-unknown))
              (make-person "Jean Cloward" 1950 "brown" (make-unknown) (make-unknown)))
              (make-person "Shannon Cloward" 1970 "brown"
              (make-person "Kirk Cloward" 1950 "brown" (make-unknown) (make-unknown))
              (make-person "Paula Cloward" 1950 "brown" (make-unknown) (make-unknown)))))


