#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

(define-type WAE
  [num (n number?)]
  [binop (op procedure?)
         (lhs WAE?)
         (rhs WAE?)]
  [with (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

;op-table
; : (listof (list/c symbol (number number . -> . number)))
(define op-table
  (list (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)))

;------------------------------------------------------------------------------------------------------------------------------------------------------------------

;(lookup-op op) → (or/c procedure? false/c)
;  op : symbol
; function that extracts the definition of an operator or false
(define (lookup-op op)
  (if (assoc op op-table)
      (second (assoc op op-table))
      false))

;Test for lookup-op
(test (lookup-op '+) +)
(test (lookup-op '-) -)
(test (lookup-op '*) *)
(test (lookup-op '/) /)
(test (lookup-op '?) #f)
(test (lookup-op '&) #f)
(test (lookup-op '@) #f)

;-----------------------------------------------------------------------------------------------------------------------------------------------------------------

;(parse s-exp) → WAE
; s-exp : s-expression
;Parses s-exp into a WAE according to this grammar:
;  WAE	 	=	 	number
; 	 	|	 	(+ WAE WAE)
; 	 	|	 	(- WAE WAE)
; 	 	|	 	(* WAE WAE)
; 	 	|	 	(/ WAE WAE)
; 	 	|	 	(with ([id WAE] ...) WAE)
; 	 	|	 	id
(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (if(or (lookup-op s-exp) (equal? 'with s-exp))
                        (error "Illegal syntax: illegal id")
                        (id s-exp))]
    [(list? s-exp)
     (if (= 3 (length s-exp))
         (cond
           [(lookup-op (first s-exp)) (binop (lookup-op (first s-exp))
                                             (parse (second s-exp))
                                             (parse (third s-exp)))]
           [(symbol=? 'with (first s-exp)) (with
                                            (parse-with-check (second s-exp))
                                            (parse (third s-exp)))])
         (error "Illegal syntax: incorrect number of pieces in expression"))]
    [else (error "Illegal syntax: Can only parse a number, symbol, or with")]))

;(parse-with-check binding-list)
;binding-list : A list of bindings
;Gatekeeper that makes sure the binding list is not empty
(define (parse-with-check binding-list)
  (if (empty? binding-list)
      (error "Illegal syntax")
      (parse-with binding-list)))

;(parse-with binding-list)
;binding-list : A list of bindings
;Helper function for the parse procedure. Helps parse the binding list
(define (parse-with binding-list)
  (if (empty? binding-list)
      empty
      (if (list? (first binding-list))
          (if (symbol? (first (first binding-list)))
              (if (or (lookup-op (first (first binding-list))) (symbol=? 'with (first (first binding-list))))
                  (error "Illegal syntax1");illegal binding id)
                  (if (= 2 (length (first binding-list)))
                      (cons (binding (first (first binding-list))(parse (second (first binding-list))))
                            (parse-with (rest binding-list)))
                      (error "Illegal syntax: Binding must contain 2 items")))
              (error "Illegal sytax: first item must be a symbol"))
          (error "Illegal syntax"))))

;Test cases for Parse:
(test (parse '5) (num 5))
(test/exn (parse true) "Illegal syntax: Can only parse a number, symbol, or with")
(test (parse '(+ 1 2)) (binop + (num 1) (num 2)))
(test (parse '(- 3 4)) (binop - (num 3) (num 4)))
(test (parse '(* 3 4)) (binop * (num 3) (num 4)))
(test (parse '(/ 3 4)) (binop / (num 3) (num 4)))
(test/exn (parse '(+ 1 2 3)) "Illegal syntax: incorrect number of pieces in expression")
(test/exn (parse '(- 1)) "Illegal syntax: incorrect number of pieces in expression")

;Test cases for Parse-Feature with:
(test (parse '(with ([x 1] [x 2]) (+ x x)))
      (with (list (binding 'x (num 1))
                  (binding 'x (num 2)))
            (binop + (id 'x) (id 'x))))
(test/exn (parse '(with ([x 1] [x 2])))
          "Illegal syntax: incorrect number of pieces in expression")
(test/exn (parse '(with ([x 1] [x 2])(+ x x)(+ x x)))
          "Illegal syntax: incorrect number of pieces in expression")
(test/exn (parse '(with x (+ 1 x) (+ 2 x))) "Illegal syntax")
(test/exn (parse '(with (x 5) (+ 1 x) (+ 2 x))) "Illegal syntax")
(test/exn (parse '(with ((x)) (+ 1 x))) "Illegal syntax")
(test/exn (parse '(with ((x 5 6)) (+ 1 x))) "Illegal syntax: Binding must contain 2 items")
(test/exn (parse '(with ((42 5)) (+ 1 x))) "Illegal sytax: first item must be a symbol")

;Test cases for Parse-Feature Id:
(test (parse 'a) (id 'a))
(test/exn (parse '+) "Illegal syntax: illegal id");
(test/exn (parse '-) "Illegal syntax: illegal id");
(test/exn (parse '/) "Illegal syntax: illegal id");
(test/exn (parse '*) "Illegal syntax: illegal id");
(test/exn (parse 'with) "Illegal syntax: illegal id");
(test/exn (parse '()) "Illegal syntax: incorrect number of pieces in expression")

;---------------------------------------------------------------------------------------------------------------------------------------------------------------

;(subst* lob body) -> WAE
;lob : (listof Binding)
;body : WAE
;Substitues for all the bindings in lob inside body simutaneously
(define (subst* lob body)
  (if (binding-list-repeat? lob)
      (error "Illegal Syntax: repeat syntax")
      (foldr (lambda (x y) (subst y (binding-name x)(num (calc (binding-named-expr x))))) body lob)))

;(subst expr sub-id val) -> list
;expr : WAE
;sub-id : symbol
;val : num
(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [binop (op l r)(binop op
                          (subst l sub-id val)
                          (subst r sub-id val))]
    [with (bindings-list bound-body)
          (if (has-id? bindings-list sub-id)
              (with (subst-bl bindings-list sub-id val)
                    bound-body)
              (with (subst-bl bindings-list sub-id val)
                    (subst bound-body sub-id val)))]))

;(has-id? bindings-list sub-id) -> boolean
;bindings-list : list of bindings
;sub-id : symbol
;Returns true if sub-id is in bindings-list, false otherwise
(define (has-id? bindings-list sub-id)
  (if (empty? bindings-list)
      false
      (if (symbol=? (binding-name (first bindings-list)) sub-id)
          true
          (has-id? (rest bindings-list) sub-id))))

;(binding-list-repeat lob) -> boolean
;lob: list of bindings
;Returns false if a symbol in the binding is repeated, true otherwise
(define (binding-list-repeat? lob)
  (if(empty? lob)
     false
     (if (has-id? (rest lob) (binding-name (first lob)))
         true
         (binding-list-repeat? (rest lob)))))

;(subst-bl bl sub-id val) -> list
;bl : binding list
;sub-d : symbol
;val : num
(define (subst-bl bl sub-id val)
  (if (empty? bl)
      empty
      (cons (binding (binding-name (first bl)) (subst (binding-named-expr (first bl)) sub-id val))
            (subst-bl (rest bl) sub-id val))))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------

;(calc e) → number?
; e : WAE?
;Consumes a WAE representation of an expression and computes the corresponding numerical result, eagerly.
(define (calc e)
  (type-case WAE e
    [num (n) n]
    [id (v) (error 'calc "free identifier")];
    [binop (op l r) (if(equal? (calc r) 0)
                       (error "Syntax error: Can't do division by zero")
                       (op (calc l)(calc r)))]
    [with (binding-list body)
          (calc (subst* binding-list body))]))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------

; run-time helper
(define (run x)
  (calc (parse x)))
;----------------------------------------------------------------------------------------------------------------------------------------------------------------

;Tests for subst*
(test (subst* (list (binding 'x (num 1))) (id 'x))
      (num 1))
(test (subst* (list (binding 'x (num 1))
                    (binding 'y (num 2)))
              (binop + (id 'x) (id 'y)))
      (binop + (num 1) (num 2)))


;Test for calc
(test (run '(with ([x 1] [y 2]) (+ x y))) 3)
(test (run '5) 5)
(test (run '(+ 2 3)) 5)
(test (run '(- 60 48)) 12)
(test (run '(* 12 5)) 60)
(test (run '(/ 20 2)) 10)
(test/exn (run '(/ 34 0)) "Syntax error: Can't do division by zero")
(test (calc (parse '(with ([x 6]) x))) 6)
(test/exn (calc (parse 'x)) "calc: free identifier")
(test (run '(with ((x 5)) (+ x 5))) 10)
(test (run '(with ((x 5)) (with ((x 6)) (+ x 5)))) 11)
(test (run '(with ((x 5)) (with ((x (+ x 1))) (+ x 5)))) 11)
(test/exn (run '(with ((x 5) (x 6)) (+ x x))) "Illegal Syntax: repeat syntax") 
