#lang plai/collector
;Daniel Ribble
;Paul Cloward
;Josh Katuka

(define half-heap-size 'uninitialized-head-size)
(define heap-ptr 'uninitialized-heap-ptr)
(define from-ptr 'uninitialized-from-ptr)
(define to-ptr 'uninitialized-to-ptr)

;(current-heap-end) -> void
; If the to-ptr is at the zero index of the heap, it sets the from-ptr to half the size of the heap. Else it sets
; the from-ptr to the heap-size.
(define (current-heap-end)
  (if (= to-ptr 0)
      half-heap-size
      (heap-size)))

(define (init-allocator)
  (begin
    (set! half-heap-size (/ (heap-size) 2))
    (set! heap-ptr 0)
    (set! to-ptr 0)
    (set! from-ptr half-heap-size)))

(define (gc:alloc-flat p)
  (begin
    (when (> (+ heap-ptr 2) (current-heap-end))
      (stopAndCopy))
    (if (procedure? p)
        (heap-set! heap-ptr 'proc)
        (heap-set! heap-ptr 'prim))
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 3) (current-heap-end))
      (stopAndCopy))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (if (gc:cons? a)
      (heap-ref (+ 1 a))
      (error 'first "expects address of cons")))

(define (gc:rest a)
  (if (gc:cons? a)
      (heap-ref (+ 2 a))
      (error 'rest "expects address of cons")))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (if (gc:cons? a)
      (heap-set! (+ 2 a) r)
      (error 'set-rest! "expects address of cons")))

(define (gc:flat? a)
  (or (eq? (heap-ref a) 'prim)
      (eq? (heap-ref a) 'proc)))

(define (gc:deref a)
  (if (gc:flat? a)
      (heap-ref (+ 1 a))
      (error 'deref "expects a primitive or procedure")))

;(getForwardAddress a) -> index of forward
;a: forward
;Get function that returns the address of the forward. If not a forward, will return error 
(define (getForwardAddress a)
  (if (forward? a)
      (heap-ref (+ 1 a))
      (error 'getForwardAddress "expecting a forward")))

;(forward? a) -> boolean
;a: forward
;Returns true if a is a forward, else false
(define (forward? a)
  (eq? (heap-ref a) 'forward))

;(prim? a) -> boolean
;a: primitive
;Returns true if a is a primitive, else false
(define (prim? a)
  (eq? (heap-ref a) 'prim))

;(proc? a) -> boolean
;a: procedure
;Returns true if a is a procedure, else false
(define (proc? a)
  (eq? (heap-ref a) 'proc))

;(stopAndCopy) -> void
;Calls the switch-to-from-ptrs and the copy-root-set functions
(define (stopAndCopy)
  (begin
    (define root-set (get-root-set))
    (switch-to-from-ptrs)
    (copy-root-set root-set)))

;(switch-to-from-ptrs) -> head-ptr
;Switches the addresses of the from-ptr and to-ptr with one another. Then sets the head-ptr to address the current to-ptr.
;Will return the head-ptr. 
(define (switch-to-from-ptrs)
  (begin
    (define temp to-ptr)
    (set! to-ptr from-ptr)
    (set! from-ptr temp)
    (set! heap-ptr to-ptr)))

;(copy-root-set root-set) -> root-set
;root-set: list of root ptrs
;When the root-set is not empty, it will call copy-root on every element in root-set
(define (copy-root-set root-set)
  (when (not(empty? root-set))
    (begin
      (copy-root (first root-set))
      (copy-root-set (rest root-set)))))

;(copy-root root) -> void
;root: ptr
;sets the forward address of of root into root
(define (copy-root root)
  (define loc (read-root root))
  (if (forward? loc)
      (set-root! root (getForwardAddress loc))
      (begin
        (copy-object loc)
        (set-root! root (getForwardAddress loc)))))

;(copy-object loc) -> void
;loc: flat or cons
;Checks to see if loc is a flat or cons. Changes the value of the head-ptr
(define (copy-object loc)
  (cond
    [(gc:flat? loc)
     (when (> (+ heap-ptr 2) (current-heap-end))
       (error 'copy-object "current-heap out of memory"))
     (begin
       (heap-set! heap-ptr (heap-ref loc))
       (heap-set! (+ heap-ptr 1) (gc:deref loc))
       (make-forward loc heap-ptr)
       (set! heap-ptr (+ heap-ptr 2))
       (when (proc? loc)
         (copy-root-set (procedure-roots (gc:deref loc)))))]
    [(gc:cons? loc)
     (when (> (+ heap-ptr 3) (current-heap-end))
       (error 'copy-object "current-heap out of memory"))
     (begin
       (define cons-loc heap-ptr)
       (heap-set! heap-ptr (heap-ref loc))
       (gc:set-first! heap-ptr (gc:first loc))
       (gc:set-rest! heap-ptr (gc:rest loc))
       (make-forward loc heap-ptr)
       (set! heap-ptr (+ heap-ptr 3))
       (copy-object (gc:first cons-loc))
       (copy-object (gc:rest cons-loc))
       (gc:set-first! cons-loc (getForwardAddress (gc:first cons-loc)))
       (gc:set-rest! cons-loc (getForwardAddress (gc:rest cons-loc))))]))

;(make-forward loc ptr) ->
;loc: flat or cons
;ptr: pointer
;Sets the loc as a 'forward and sets the location after loc on the heap to be ptr
(define (make-forward loc ptr)
  (begin
    (heap-set! loc 'forward)
    (heap-set! (+ loc 1) ptr)))



;Tests
(test (with-heap (make-vector 20)
                 (init-allocator)
                 (gc:deref (gc:alloc-flat 2)))
      2)

(test (with-heap (make-vector 20)
                 (init-allocator)
                 (heap-ref (gc:alloc-flat 2)))
      'prim)

(test (with-heap (make-vector 20)
                 (init-allocator)
                 (define a (gc:deref (gc:alloc-flat 'a)))
                 (define b (gc:deref (gc:alloc-flat 'b)))
                 (define c (gc:deref (gc:alloc-flat 'c)))
                 (define d (gc:deref (gc:alloc-flat 'd)))
                 a)
      'a)

(test (with-heap (make-vector 20)
                 (init-allocator)
                 (define a (gc:deref (gc:alloc-flat 'a)))
                 (define b (gc:deref (gc:alloc-flat 'b)))
                 (define c (gc:deref (gc:alloc-flat 'c)))
                 (define d (gc:deref (gc:alloc-flat 'd)))
                 (define e (gc:deref (gc:alloc-flat 'e)))
                 (define f (gc:deref (gc:alloc-flat 'f)))
                 (define g (gc:deref (gc:alloc-flat 'g)))
                 (define h (gc:deref (gc:alloc-flat 'h)))
                 (define i (gc:deref (gc:alloc-flat 'i)))
                 (define j (gc:deref (gc:alloc-flat 'j)))
                 (define k (gc:deref (gc:alloc-flat 'k)))
                 (define l (gc:deref (gc:alloc-flat 'l)))
                 (define m (gc:deref (gc:alloc-flat 'm)))
                 (define n (gc:deref (gc:alloc-flat 'n)))
                 (begin
                   a
                   b
                   c
                   d
                   e
                   n))
      'n)