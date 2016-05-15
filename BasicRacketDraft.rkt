;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |basic racket assignment|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (sum-coins pennies nickels dimes quarters)
  (+ (* pennies .01)
     (* nickels .05)
     (* dimes .1)
     (* quarters .25)))

(define (area-cylinder base-radius height)
  (+ (* 2 pi base-radius height)
     (* 2 pi base-radius base-radius)))

(check-within (area-cylinder 2 2) 50.265 .01)
(check-within (area-cylinder 5 18) 722.57 .01)

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


(define (netpay hours-worked)
  (-
   (* 12 hours-worked)
   (tax (* 12 hours-worked))))

(define (what-kind a b c)
  (if (= a 0)
      'degenerative'
      (cond
        [(> (* b b)(* 4 a c) 'two']
        [(= (* b b) (* 4 a c)) 'one']
        [else 'none']
        )))
