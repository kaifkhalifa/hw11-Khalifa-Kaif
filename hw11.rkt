#lang racket

(provide (all-defined-out))

(require rackunit)

;; Data Definitions

;; A CS450LangExpr (Expr) is one of:
;; - Number
;; - String
;; - 'TRUE
;; - 'FALSE
;; - (list '=== Expr Expr)
;; - (list Expr '? Expr ': Expr)
;; - (list '+ Expr Expr)
;; - (list '- Expr Expr)
;; Interpretation: Represents surface-level syntax of CS450Lang programs.

;; CS450LangExpr? : Any -> Boolean
;; Returns true if a value is a valid CS450LangExpr.
(define (CS450LangExpr? expr)
  (or (number? expr)
      (string? expr)
      (eq? expr 'TRUE)
      (eq? expr 'FALSE)
      (and (list? expr)
           (match expr
             [`(+ ,x ,y) (and (CS450LangExpr? x) (CS450LangExpr? y))]
             [`(- ,x ,y) (and (CS450LangExpr? x) (CS450LangExpr? y))]
             [`(=== ,x ,y) (and (CS450LangExpr? x) (CS450LangExpr? y))]
             [`(,cond '? ,true-expr ': ,false-expr) 
              (and (CS450LangExpr? cond) 
                   (CS450LangExpr? true-expr) 
                   (CS450LangExpr? false-expr))]
             [_ #false]))))

;; A CS450LangAST (AST) is one of:
;; - (num Number)
;; - (str String)
;; - (bool Boolean)
;; - (add AST AST)
;; - (sub AST AST)
;; - (equals AST AST)
;; - (cond-expr AST AST AST)
;; Interpretation: Represents an abstract syntax tree for CS450Lang.

(struct num [val] #:transparent)
(struct str [val] #:transparent)
(struct bool [val] #:transparent)
(struct add [left right] #:transparent)
(struct sub [left right] #:transparent)
(struct equals [left right] #:transparent)
(struct cond-expr [cond true-branch false-branch] #:transparent)

;; A 450LangResult (Result) is a
;; - Number
;; - String
;; - Boolean
;; - NaN
(struct nan [])
(define NaN (nan))
(define (Result? r)
  (or (number? r)
      (string? r)
      (boolean? r)
      (NaN? r)))


;; parse : CS450LangExpr -> CS450LangAST
;; Parses a surface-level CS450Lang expression into an abstract syntax tree.
(define/contract (parse expr)
  (-> CS450LangExpr? any/c)
  (match expr
    [(? number?) (num expr)]
    [(? string?) (str expr)]
    ['TRUE (bool #true)]
    ['FALSE (bool #false)]
    [`(+ ,x ,y) (add (parse x) (parse y))]
    [`(- ,x ,y) (sub (parse x) (parse y))]
    [`(=== ,x ,y) (equals (parse x) (parse y))]
    [`(,cond '? ,true-expr ': ,false-expr) 
     (cond-expr (parse cond) (parse true-expr) (parse false-expr))]
    [_ NaN]))

;; run : CS450LangAST -> CS450LangResult
;; Evaluates a CS450LangAST and returns the result.
(define/contract (run ast)
  (-> any/c any/c)
  (match ast
    [(num n) n]
    [(str s) s]
    [(bool b) b]
    [(add x y) (450+ (run x) (run y))]
    [(sub x y) (if (and (number? (run x)) (number? (run y)))
                   (- (run x) (run y))
                   NaN)]
    [(equals x y) (if (equal? (run x) (run y)) #true #false)]
    [(cond-expr cond true-branch false-branch)
     (if (truthy? (run cond)) (run true-branch) (run false-branch))]
    [_ NaN]))

;; 450+ : CS450LangResult CS450LangResult -> CS450LangResult
;; Adds two results, either numerically or as strings.
(define/contract (450+ x y)
  (-> any/c any/c any/c)
  (cond
    [(or (NaN? x) (NaN? y)) NaN]
    [(and (number? x) (number? y)) (+ x y)]
    [(and (number? x) (string? y)) (string-append (number->string x) y)]
    [(and (string? x) (number? y)) (string-append x (number->string y))]
    [(and (string? x) (string? y)) (string-append x y)]
    [else NaN]))

;; NaN? : CS450LangResult -> Boolean
;; Returns true if the result is NaN.
(define/contract (NaN? result)
  (-> any/c boolean?)
  (equal? result NaN))

;; truthy? : Any -> Boolean
;; Determines if a value is truthy (mimicking JavaScript semantics).
(define/contract (truthy? val)
  (-> any/c boolean?)
  (cond
    [(or (eq? val #false) (eq? val NaN)) #false]
    [(or (equal? val 0) (equal? val "") (equal? val '())) #false]
    [else #true]))

;; eval-ssexpr : CS450LangExpr -> CS450LangResult
;; Combines parse and run to evaluate surface expressions.
(define eval-ssexpr (compose run parse))

;; Test cases for each function

;; Examples for CS450LangExpr?
(check-equal? (CS450LangExpr? '(+ 1 2)) #true)
(check-equal? (CS450LangExpr? '(=== "hello" "world")) #true)

;; Examples for parse
(check-equal? (parse '(+ 1 2)) (add (num 1) (num 2)))
(check-equal? (parse '(=== "hello" "world")) (equals (str "hello") (str "world")))

;; Examples for run
(check-equal? (run (add (num 1) (num 2))) 3)
(check-equal? (run (equals (num 5) (num 5))) #true)

;; Examples for truthy?
(check-equal? (truthy? 0) #false)
(check-equal? (truthy? "hello") #true)

;; Examples for 450+
(check-equal? (450+ 1 2) 3)
(check-equal? (450+ "hello" 3) "hello3")

;; Examples for NaN?
(check-equal? (NaN? NaN) #true)
(check-equal? (NaN? 5) #false)

;; Examples for eval-ssexpr
(check-equal? (eval-ssexpr '(+ 1 2)) 3)
(check-equal? (eval-ssexpr '(=== "hello" "hello")) #true)