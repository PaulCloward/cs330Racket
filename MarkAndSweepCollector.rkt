#lang plai/collector
;Daniel Ribble
;Paul Cloward
;Josh Katuka

(define heap-ptr 'uninitialized-heap-ptr)
(define free-list 'uninitialized-free-list)

(define (init-allocator)
  (set! free-list (build-free-list 0)))

(define (build-free-list index)
  (if (> (+ index 4) (heap-size))
      empty
      (cons index (build-free-list (+ index 4)))))

(define (gc:alloc-flat p)
  (begin
    (when (empty? free-list)
      (markAndSweep))
    (set! heap-ptr (first free-list))
    (set! free-list (rest free-list))
    (if (procedure? p)
        (heap-set! heap-ptr 'proc)
        (heap-set! heap-ptr 'prim))
    (heap-set! (+ 1 heap-ptr) 0)
    (heap-set! (+ 2 heap-ptr) p)
    heap-ptr))

(define (gc:cons f r)
  (begin
    (when (empty? free-list)
      (markAndSweep))
    (set! heap-ptr (first free-list))
    (set! free-list (rest free-list))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) 0)
    (heap-set! (+ 2 heap-ptr) f)
    (heap-set! (+ 3 heap-ptr) r)
    heap-ptr))

(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

(define (gc:first a)
  (if (gc:cons? a)
      (heap-ref (+ 2 a))
      (error 'first "expects address of cons")))

(define (gc:rest a)
  (if (gc:cons? a)
      (heap-ref (+ 3 a))
      (error 'rest "expects address of cons")))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 2 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (if (gc:cons? a)
      (heap-set! (+ 3 a) r)
      (error 'set-rest! "expects address of cons")))

(define (gc:flat? a)
  (or (eq? (heap-ref a) 'prim)
      (eq? (heap-ref a) 'proc)))

(define (gc:deref a)
  (if (gc:flat? a)
      (heap-ref (+ 2 a))
      (error 'deref "expects a primitive or procedure")))

;(getMark a) -> num
;a: num
;returns the value at where index a is located on the heap
(define (getMark a)
  (heap-ref (+ 1 a)))

;(setMark a val) -> void
;a: num
;val: num
;Sets the val to the index a + 1 on the heap
(define (setMark a val)
  (heap-set! (+ 1 a) val))

;(prim? a) -> boolean
;a: symbol
;Returns true if a is 'prim else returns false
(define (prim? a)
  (eq? (heap-ref a) 'prim))

;(proc? a) -> boolean
;a: symbol
;Returns true if a is 'proc else returns false
(define (proc? a)
  (eq? (heap-ref a) 'proc))

;(markAndSweep) -> void
;Goes through the heap and Marks and Sweeps. First marks the elements in the heap. Then
;calls the sweep function starting at the beginning of the heap. If the free-list is out of memory, returns an error
(define (markAndSweep)
  (define root-set (get-root-set))
  (begin
    (mark root-set)
    (sweep 0)
    (when (empty? free-list)
      (error 'markAndSweep "Heap Out of Memory"))))

;(mark root-set) -> void
;root-set: list of pointers
;Recursively works through root-set to mark each elements with a 1 or zero
(define (mark root-set)
  (if (empty? root-set)
      (void)
      (begin
        (mark-loc  (read-root (first root-set)))
        (mark (rest root-set)))))

;(mark-loc loc) -> void
;loc: proc or cons
;Marks the location of the cons or procedure given
(define (mark-loc loc)
  (begin
    (setMark loc 1)
    (cond
      [(proc? loc) (mark (procedure-roots (gc:deref loc)))]
      [(gc:cons? loc) (begin
                        (mark-loc (gc:first loc))
                        (mark-loc (gc:rest loc)))])))

;(sweep index) -> void
;index: num
;Recursive Function used to check indexes to see which elements are marked zero. If marked zero, added to the free-list. Else marked zero. 
(define (sweep index)
  (when (not (> (+ index 4) (heap-size)))
    (begin
      (if (= (getMark index) 0)
            (set! free-list (cons index free-list))
            (setMark index 0))
      (sweep (+ index 4)))))

;Tests
(with-heap (make-vector 100)
           (init-allocator)
           free-list)

(test (with-heap (make-vector 20)
                 (init-allocator)
                 (gc:deref (gc:alloc-flat 2)))
      2)