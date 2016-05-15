#lang plai

(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (lob (listof Binding?)) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

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

;environment
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

;return values
(define-type CFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?))
            (body CFWAE?)
            (env Env?)])



;parse:expression ->CFWAE
;This procedure parses an expression into a CFWAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)];Was isLegalId
    [(list? sexp)
     (if (isCorrectSize? sexp)
         (cond
           [(lookup-op (first sexp)) (binop (lookup-op (first sexp))
                                            (parse (second sexp))
                                            (parse (third sexp)))]
           [(equal? 'with (first sexp)) (with (parse-with-check (second sexp))
                                              (parse (third sexp)))]
           [(equal? 'fun (first sexp)) (fun (checkFunArgs (second sexp))
                                            (parse (third sexp)))]
           [(equal? 'if0 (first sexp)) (if0 (parse  (second sexp))
                                            (parse (third sexp))
                                            (parse (fourth sexp)))]
           [else (app (parse (first sexp))
                      (parse-app-args (rest sexp)))])
         (error "Illegal syntax: incorrect number of arguments"))]
    [else (error "Illegal syntax: Invalid Input")]))


;(isCorrectSize? sexp-list) -> boolean
;sexp-list : list
;checks to see if the sexp-list is the correct size
(define (isCorrectSize? sexp-list)
  (cond
    [(empty? sexp-list) false]
    [(or (lookup-op (first sexp-list))
         (equal? 'with (first sexp-list))
         (equal? 'fun (first sexp-list))) (if (= 3 (length sexp-list))
                                              true
                                              false)]
    [(equal? 'if0 (first sexp-list)) (if (= 4 (length sexp-list))
                                         true
                                         false)]
    [else (if (<= 1 (length sexp-list))
              true
              false)]))





;Gatekeepter that makes sure that the binding list is not empty before passing to parse-with function.
(define (parse-with-check binding-list)
  (if (list? binding-list)
      (if (empty? binding-list)
          (error "Illegal syntax: binding list cannot be empty")
          (let ([lob (parse-with binding-list)])
            (if (bindings-list-repeat? lob)
                (error "Illegal syntax: repeated symbol is binding list")
                lob)))
      (error "Illegal syntax: binding list must be list")))



;Defining a parse-with function to go through the binding list and parse the second-sexp with list, symbols
;and lookup-op.
(define (parse-with binding-list) 
  (if (empty? binding-list)
      empty
      (if (list? (first binding-list))
          (if (symbol? (first (first binding-list)))
              (if (isLegalId? (first (first binding-list)));isLegalId
                  (if (= 2 (length (first binding-list)))
                      (cons (binding (first (first binding-list)) (parse (second (first binding-list))))
                            (parse-with (rest binding-list)))
                      (error "Illegal syntax: binding must contain two args"))
                  (error "Illegal syntax: illegal binding id"))
              (error "Illegal syntax: illegal binding id"))
          (error "Illegal syntax"))))

(define (isLegalId? id)
  (if (or (lookup-op id)
          (symbol=? 'with id)
          (symbol=? 'fun id)
          (symbol=? 'if0 id))
      false
      true))

;(define (has-id? bindings-list sub-id))
;Function to check if the sub-id is contained within the binding-list.
(define (has-id? bindings-list sub-id)
  (if (empty? bindings-list)
      false
      (if (symbol=? (binding-name (first bindings-list)) sub-id)
          true
          (has-id? (rest bindings-list) sub-id))))



;(define (bindings-list-repeat? lob))
;Function to check if the bindings-list repeats within a certain list of bindings.
(define (bindings-list-repeat? lob)
  (if (empty? lob)
      false
      ;(rest lob)
      (if (has-id? (rest lob) (binding-name (first lob)))
          true
          (bindings-list-repeat? (rest lob)))))


;Checks to see if args while parsing fun contains no errors
(define (checkFunArgs args)
  (if (list? args)
      (if (isLegalArgs? args)
          args
          (error "Error"))
      (error "Illegal syntax: fun args must be list of ids")))



;Checks if args has repeated or illegal symbols, throws error if so.
(define (isLegalArgs? args)
  (if (empty? args)
      true
      (if (symbol? (first args))
          (if (isLegalId? (first args))
              (if (isFound? (first args) (rest args))
                  (error "Illegal syntax: repeated symbol")
                  (isLegalArgs? (rest args)))
              (error "Illegal syntax: illegal id in args"))
          (error "Illegal syntax: args must be symbols"))))


(define (isFound? arg-id args)
  (if (empty? args)
      false
      (if (equal? arg-id (first args))
          true
          (isFound? arg-id (rest args)))))


;Test checkFunArgs
(test/exn (checkFunArgs '(x + bob)) "illegal id in args")
(test/exn (checkFunArgs '(x 5)) "Illegal syntax: args must be symbols")
(test/exn (checkFunArgs '(x x)) "Illegal syntax")


;parse-app-args : args -> list of CFWAE
;   args : list
;parses the parameters of an app and passes back a list of CFWAE
(define (parse-app-args args)
  (if (empty? args)
      empty
      (cons (parse (first args))
            (parse-app-args (rest args)))))


;Tests for Parse-----------------------------------------------------------------------------------------------------------------------------------------------
(test (parse '5) (num 5))
(test/exn (parse "byu") "Illegal syntax: Invalid Input")

(test (parse '(+ 6 1)) (binop + (num 6)(num 1)))
(test (parse '(- 9 2)) (binop - (num 9)(num 2)))
(test (parse '(* 4 3)) (binop * (num 4)(num 3)))
(test (parse '(/ 9 2)) (binop / (num 9)(num 2)))
(test/exn (parse '(/ 9 3 2)) "Illegal syntax: incorrect number of arguments")
(test/exn (parse '(+ 1)) "Illegal syntax: incorrect number of arguments")

(test (parse 'x)(id 'x))
(test/exn (parse '(-)) "Illegal syntax:")
(test/exn (parse '(+)) "Illegal syntax:")
(test/exn (parse '(/)) "Illegal syntax:")
(test/exn (parse '(*)) "Illegal syntax:")
(test/exn (parse '(with)) "Illegal syntax:")
(test/exn (parse '(if0)) "Illegal syntax:")
(test/exn (parse '(fun)) "Illegal syntax:")

(test (parse '(if0 1 1 1)) (if0 (num 1) (num 1) (num 1)))
(test/exn (parse '(if0 1 1)) "Illegal syntax:")
(test/exn (parse '(if0 1 1 1 1)) "Illegal syntax:")

(test (parse '(with ([x 1][y 2]) (+ y x)))
      (with (list (binding 'x (num 1))
                  (binding 'y (num 2)))
            (binop + (id 'y)(id 'x))))
(test/exn (parse '(with [(x 5)])) "Illegal syntax: incorrect number of arguments")
(test/exn (parse '(with [(x 5) x x])) "Illegal syntax:")
(test/exn (parse '(with x (+ 7 x))) "Illegal syntax")
(test/exn (parse '(with (x 5)(+ 5 x))) "Illegal syntax")
(test/exn (parse '(with (x)(+ 5 x))) "Illegal syntax")
(test/exn (parse '(with (x 5 10)(+ 7 x))) "Illegal syntax")
(test/exn (parse '(with (x 8)(8 x))) "Illegal syntax")
(test/exn (parse '(with [(4 5)] x)) "Illegal syntax")
(test/exn (parse '(with (with 4) with)) "Illegal syntax")
(test/exn (parse '(with [(x 1)(x 3)] x)) "Illegal syntax")


(test (parse '(fun (x) (+ x 6)))
      (fun '(x) (binop + (id 'x) (num 6))))
(test (parse '(fun() ( + 5 6)))
      (fun '() (binop + (num 5)(num 6))))
(test (parse '(fun (x y) (+ x y)))
      (fun '(x y) (binop + (id 'x) (id 'y))))
(test/exn (parse '(fun (x y))) "Illegal syntax")
(test/exn (parse '(fun (x y)(+ x y)(* x y))) "Illegal syntax")
(test/exn (parse '(fun x (+ x 1))) "Illegal syntax")
(test/exn (parse '(fun (5)(+ 5 6))) "Illegal syntax")
(test/exn (parse '(fun (with 5) with)) "Illegal syntax")
(test/exn (parse '(fun (x x) (+ x x))) "Illegal syntax")

(test (parse '((fun (x) (+ x 1)) 2))
      (app (fun '(x)(binop + (id 'x)(num 1)))(list(num 2))))

(test/exn (parse '()) "Illegal syntax")

(test (parse '((fun(x) (+ x 6)) 5))
      (app (fun '(x)(binop + (id 'x)(num 6)))(list (num 5))))

(define (numOp op n1 n2)
  (numV (op (numV-n n1) (numV-n n2))))  

(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'lookup "no binding for identifier")]
    [anEnv (bound-name bound-value rest-env)
           (if (symbol=? bound-name name)
               bound-value
               (lookup name rest-env))]))

; interp : CFWAE Env -> CFWAE-Value
; This procedure interprets the given CFWAE in the environment
; and produces a result in the form of a CFWAE-Value
(define (interp expr env)
  (type-case CFWAE expr
    [num (n) (numV n)]
    [binop (op l r) (if (and (equal? / op)
                             (equal? (numV 0) (interp r env)))
                        (error "division by zero")
                        (numOp op (interp l env) (interp r env)))]
    [id (v) (lookup v env)]
    [if0 (test truth falsity) (if (num-zero? (interp test env))
                                  (interp truth env)
                                  (interp falsity env))]
    [with (binding-list body) (interp body (make-with-env binding-list env))]
    [fun (args body) (closureV args body env)]
    [app (fun-expr args)
         (let ([fun-val (interp fun-expr env)])
           (interp-app fun-val args env))]))


(define (num-zero? n)
  (if (numV? n)
      (zero? (numV-n n))
      (error "Illegal syntax: conditional statement must evaluate to a number/numV")))

;make-with-env : binding-list env -> env
;iterates through the binding list and makes additions to the environment
(define (make-with-env binding-list env)
  (foldr (lambda (x y) (anEnv (binding-name x)
                              (interp (binding-named-expr x)
                                      y)
                              y))
         env binding-list));Used to have a body variable before binding-list 

;interp-app : fun-val args env -> CFWAE-value
;checks if fun-val and args are the same size. Then applies args to fun-val
(define (interp-app fun-val args env)
  (if (= (length (closureV-params fun-val)) (length args))
      (interp (closureV-body fun-val)
              (make-app-env (closureV-params fun-val)
                            (closureV-env fun-val)
                            args
                            env))
      (error "Illegal syntax: incorrect number of arguments to match function signature")))

;make-app-env : fun-env args env -> CFWAE-value
;makes-environment to be applied to function definition when used in an app
(define (make-app-env fun-params fun-env args env)
  (if (empty? fun-params)
      fun-env
      (anEnv (first fun-params)
             (interp (first args) env)
             (make-app-env (rest fun-params)
                           fun-env
                           (rest args)
                           env))))


;run: expr -> CFWAE-Value
;Run-time helper parses then evaluates an s-expression in CFWAE language
(define (run expr)
  (interp (parse expr) (mtEnv)))

;Tests for interp-------------------------------------------------------------------

;Test for literals
(test (run '5)(numV 5))
(test (run '-3)(numV -3))

;Testing for binary operator
(test (run '(+ 5 6)) (numV 11))
(test/exn (run '(+ x 6)) "lookup: no binding for identifier")
(test/exn (run '(+ 6 x)) "lookup: no binding for identifier")
(test (run '(- 5 6)) (numV -1))
(test/exn (run '(- x 6)) "lookup: no binding for identifier")
(test/exn (run '(- 6 x)) "lookup: no binding for identifier")
(test (run '(* 5 6)) (numV 30))
(test/exn (run '(* x 6)) "lookup: no binding for identifier")
(test/exn (run '(* 6 x)) "lookup: no binding for identifier")
(test (run '(/ 9 3)) (numV 3))
(test/exn (run '(/ x 6)) "lookup: no binding for identifier")
(test/exn (run '(/ 6 x)) "lookup: no binding for identifier")
(test/exn (run '(/ 4 0)) "division by zero")

;Testing id
(test/exn (run 'w) "lookup: no binding for identifier")

;Testing if0
(test (run '(if0 1 1 1)) (numV 1))
(test (run '(if0 2 77 9)) (numV 9))
(test/exn (run '(if0 x x 2)) "lookup: no binding for identifier")

;Testing With
(test (run '(with ([x 5]) x)) (numV 5))
(test (run '(with ([x 5]) (with ([x 6]) (+ x 5)))) (numV 11))
(test (run '(with ([x 5]) (with ([x (+ x 1)]) (+ x 5)))) (numV 11))
(test (run '(with ([x 5]) (+ x 5)))(numV 10))
(test (run '(with([x 5][y 3]) ( * x y))) (numV 15))
(test (run '(with([x 6][y 10]) (+ x y))) (numV 16))
(test (run '(with([x 9][y 100]) (- x y))) (numV -91))
(test (run '(with([x 4][y 2]) (/ x y))) (numV 2))

;Testing fun
(test (run '(fun (x) x))(closureV '(x)(id 'x)(mtEnv)))
(test (run '(fun(x)(+ (with([x 5]) x) x)))
      (closureV '(x)(binop + (with(list(binding 'x(num 5)))(id 'x))
                           (id 'x))(mtEnv)))

;Testing App
(test (run '((fun (x) x) 1)) (numV 1))
(test/exn (run '(a 3 1)) "lookup: no binding for identifier")
(test/exn (run '((fun (x y) (+ x y)) 5)) "Illegal syntax")
(test/exn (run '((fun (x y) (+ x y)) 3 1 7)) "Illegal syntax")
(test (run '(with ([f (with ([x 5])
                            (fun (y) (+ x y)))])
                  (with ([x 10])
                        (f 8))))
      (numV 13))





