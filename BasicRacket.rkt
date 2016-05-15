;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Basic Racket Paul Cloward|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(sum-coins pennies nickels dimes quarters) → number
;  pennies : number
;  nickels : number
;  dimes : number
;  quarters : number
;The procedure returns the summation of the pennies nickels dimes and
;quarters given to it
(define (sum-coins pennies nickels dimes quarters)
  (+ (* pennies .01)
     (* nickels .05)
     (* dimes .1)
     (* quarters .25)))
;Test for different amount of change of each coin
(check-expect (sum-coins 2 3 1 4) 1.27)

;Test for when no coins are put in
(check-expect (sum-coins 0 0 0 0) 0)

;Test for when all coins are the same number
(check-expect (sum-coins 7 7 7 7) 2.87)

;Test for pennies
(check-expect (sum-coins 5 0 0 0) .05)

;Test for nickels
(check-expect (sum-coins 0 7 0 0) .35)

;test for dimes
(check-expect (sum-coins 0 0 4 0) .4)

;test for quarters
(check-expect (sum-coins 0 0 0 4) 1.00)

;(area-cylinder base-radius height) → number
;  base-radius : number
;  height : number
;Returns the surface area of a cylinder
(define (area-cylinder base-radius height)
  (+ (* 2 pi base-radius height)
     (* 2 pi base-radius base-radius)))

(check-within (area-cylinder 2 2) 50.265 .01)
(check-within (area-cylinder 5 18) 722.57 .01)

;(tax gross-pay) → number
;  gross-pay : number
;Produces the amount of tax owed. For a gross pay of $240 or less,
;the tax is 0%; for over $240 and $480 or less, the tax rate is 15%;
;and for any pay over $480, the tax rate is 28%.
(define (tax gross-pay)
  (cond  [(> gross-pay 480) (* gross-pay .28)]
    [(> gross-pay 240) (* gross-pay .15)]
    [else 0]
))

;under 240
(check-expect (tax 50) 0)
;at 240
(check-expect (tax 240) 0)
;at 241
(check-expect (tax 241) 36.15)
;at 300
(check-expect (tax 300) 45)
;at 480
(check-expect (tax 480) 72)
;at 481
(check-expect (tax 481) 134.68)
;at 500
(check-expect (tax 500) 140)

;(netpay hours-worked) → number
;  hours-worked : number
;Determines the net pay of an employee, which is the gross pay minus
;the tax. Assume the hourly pay rate is $12.
(define (netpay hours-worked)
  (-
   (* 12 hours-worked)
   (tax (* 12 hours-worked))))

;10 hours worked. There should be 0 dollars taxed
(check-expect (netpay 10) 120)

;21 hours worked. There should be .15 taxed off the income
(check-expect (netpay 21) 214.2)

;42 hours worked. There should be .28 taxed off the income
(check-expect (netpay 42) 362.88)

;(what-kind a b c) → symbol
;  a : number
;  b : number
;  c : number
;The function consumes the coefficients a, b, and c of a quadratic
;equation. It then determines whether the equation is degenerate and,
;if not, how many solutions the equation has. The function produces one
;of four symbols: 'degenerate, 'two, 'one, or 'none. An equation is
;degenerate if (= a 0).
(define (what-kind a b c)
  (if (= a 0)
      "degenerative"
      (cond
        [(> (* b b)(* 4 a c)) "two"]
        [(= (* b b) (* 4 a c)) "one"]
        [else "none"]
        )))

;the degenerative test case
(check-expect (what-kind 0 2 3) "degenerative")

;two solution test case
(check-expect (what-kind 1 3 1) "two")

;one solution test case
(check-expect (what-kind 1 2 1) "one")

;none test case
(check-expect (what-kind 3 1 3) "none")


;(struct time (hours minutes seconds))
;  hours : number
;  minutes : number
;  seconds : number
 (define-struct time (hours minutes seconds))

;(time-diff t1 t2) → number
;  t1 : time
;  t2 : time
;Returns the number of seconds from t1 to t2.
 (define (time-diff time1 time2)
   (+
    [- (* (time-hours time2) 3600 )
      (* (time-hours time1) 3600)]
    [- (* (time-minutes time2) 60)
       (* (time-minutes time1) 60)]
    [-(time-seconds time2) (time-seconds time1)]
    ))

 ;check when theyre the same times
 (define t1 (make-time 1 2 2))
 (define t2 (make-time 1 2 2))
(check-expect (time-diff t1 t2) 0)

;check when the second time is much further from midnight than the first time
 (define time1 (make-time 1 1 1))
 (define time2 (make-time 100 100 100))
(check-expect (time-diff time1 time2) 362439)


;(struct position (x y))
;  x : number
;  y : number
 (define-struct position (x y))

;(struct circ (center radius))
;  center : position
;  radius : number
 (define-struct circ (center radius))

;(struct rect (upper-left width height))
;  upper-left : position
;  width : number
;  height : number
 (define-struct square (upper-left length))

;(struct rect (upper-left width height))
;  upper-left : position
;  width : number
;  height : number
 (define-struct rect (upper-left width height))

;(area shape) → number
;  shape : (or/c circ square rect)
;Computes the area of the shape
 (define (area shape)
   (cond
     [(circ? shape) (* pi (circ-radius shape) (circ-radius shape))]
     [(square? shape) (* (square-length shape) (square-length shape))]
     [(rect? shape)(* (rect-width shape) (rect-height shape))]
     [else "not a correct shape"]
     ))     

;Test case for the area of a circle with radius 3
(define circle1 (make-circ 3 3))
(check-within (area circle1) 28.2743 .01)

;Test case for the area of a square with length 12
(define square1 (make-square 0 12))
(check-expect (area square1) 144)

;Test case for the area of a rectangle with with 12 and length 34
(define rectangle1 (make-rect 0 12 34))
(check-expect (area rectangle1) 408)

;test invalid
(check-expect (area 5) "not a correct shape") 

;(translate-shape shape delta) → (or/c circ square rect)
;  shape : (or/c circ square rect)
;  delta : number
;Produces a shape whose key position is moved by delta pixels in the x direction.
(define (translate-shape shape delta)
  (cond
    [(circ? shape) (make-circ (make-position (+  (position-x (circ-center shape))delta)
                                             (position-y (circ-center shape)))
                              (circ-radius shape))]
    [(square? shape) (make-square (make-position (+  (position-x (square-upper-left shape))delta)
                                             (position-y (square-upper-left shape)))
                              (square-length shape))]
    [(rect? shape) (make-rect (make-position (+  (position-x (rect-upper-left shape))delta)
                                             (position-y (rect-upper-left shape)))
                              (rect-width shape)(rect-height shape))]
    [else "not a correct shape"]
    ))
   
;Test case for the area of a circle with radius 3
(define circle2 (make-circ (make-position 0 0) 3))
(define circle3 (make-circ (make-position 1 0) 3))
(check-expect (translate-shape circle2 1) circle3)

;Test case for the area of a square with length 12
(define square2 (make-square (make-position 0 0) 3))
(define square3 (make-square (make-position 1 0) 3))
(check-expect (translate-shape square2 1) square3)

;Test case for the area of a rectangle with with 12 and length 34
(define rect2 (make-rect (make-position 0 0) 3 4))
(define rect3 (make-rect (make-position 1 0) 3 4))
(check-expect (translate-shape rect2 1) rect3)

;invalid test for translate shape. Not a correct shape entered
(check-expect (translate-shape 4 1) "not a correct shape")


;(in-shape? shape p) → boolean
;  shape : (or/c circ square rect)
;  p : position
; Returns true if p is within the shape, false otherwise.
(define (in-shape? shape p)
  (if (position? p)
      (cond
   [(circ? shape) (if (< (+ (* (-(position-x p) (position-x (circ-center shape)))
                               (-(position-x p)
                                 (position-x (circ-center shape))))
                              (*
                               (- (position-y p)
                                  (position-y (circ-center shape)))
                               (- (position-y p)(position-y (circ-center shape)))))
                  (* (circ-radius shape)(circ-radius shape)))
                  true
                  false
                  )]
     [(square? shape) (if (< 
                             (- (position-x p) (position-x (square-upper-left shape)))
                             (square-length shape))                    
                          (if (< (- (position-y p)
                                    (position-y (square-upper-left shape)))
                             (square-length shape))
                              true
                              false)
                          false)]
     [(rect? shape)(if (< (- (position-x p)
                             (position-x (rect-upper-left shape)))
                             (rect-width shape))                    
                          (if (< (- (position-y p)
                                    (position-y (rect-upper-left shape)))
                             (rect-height shape))
                              true
                              false)
                          false)]
     [else "not a correct shape"]
  )
  "P is not a position"))

;test circle
(check-expect (in-shape? (make-circ (make-position 1 1) 3)
                         (make-position 2 2)) true)
(check-expect (in-shape? (make-circ (make-position 1 1) 3)
                         (make-position 5 5)) false)

;test square new point is in the shape
(check-expect (in-shape? (make-square (make-position 0 0) 3)
                         (make-position 1 1)) true)
;test square when new point is not in the shape
(check-expect (in-shape? (make-square (make-position 0 0) 3)
                         (make-position 4 2)) false)
(check-expect (in-shape? (make-square (make-position 0 0) 3)
                         (make-position 2 4)) false)

;test rectangle new point is in the shape
(check-expect (in-shape? (make-rect (make-position 0 0) 3 4)
                         (make-position 2 3)) true)
;test rectangle when new point is not in the shape
(check-expect (in-shape? (make-rect (make-position 1 1) 3 4)
                         (make-position 2 6)) false)
(check-expect (in-shape? (make-rect (make-position 1 1) 3 4)
                         (make-position 5 6)) false)
;test stupid shape
(check-expect (in-shape? 5 (make-position 2 3)) "not a correct shape")
;test stupid position
(check-expect (in-shape? (make-rect (make-position 0 0) 3 4)
                         5) "P is not a position")