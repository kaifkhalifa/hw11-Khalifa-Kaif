#lang racket

(provide (all-defined-out)) ;; Ensures that all required identifiers, including NaN?, are provided.

(require rackunit)

;; Data Definitions

;; A Variable (Var) is a Symbol

;; An Environment (Env) is a List<(list Var Result)>

;; A 450LangExpr (Expr) is one of:
;; - Number
;; - Variable (Symbol)
;; - (list 'bind [Var Expr] Expr)
;; - (cons Expr List<Expr>)
;; Interpretation: Represents surface-level syntax of CS450Lang programs.

;; A 450LangAST (AST) is one of:
;; - (num Number)
;; - (vari Symbol)
;; - (bind-ast Symbol AST AST)
;; - (call AST List<AST>)
;; Interpretation: Represents the abstract syntax tree produced from parsing.

;; A 450LangResult (Result) is one of:
;; - Number
;; - NaN
;; - UNDEFINED-ERROR
;; - NOT-FN-ERROR
;; Interpretation: Possible results after running a CS450Lang program.

;; AST Structures
(struct num [val] #:transparent)
(struct vari [name] #:transparent)
(struct bind-ast [var expr body] #:transparent)
(struct call [fn args] #:transparent)

;; Error Results
(struct nan [] #:transparent)
(struct undefined-error [] #:transparent)
(struct not-fn-error [] #:transparent)

(define NaN (nan))
(define Undefined-Error (undefined-error))
(define Not-Fn-Error (not-fn-error))

;; Predicate for NaN
;; NaN? : Any -> Boolean
;; Purpose: Determines if a given value is NaN.
;; Example: (NaN? NaN) => #true
(define (NaN? value)
  (nan? value))

;; Examples for `NaN?`
(check-equal? (NaN? NaN) #true)
(check-equal? (NaN? 42) #false)

;; Environment Functions
(define (lookup var env)
  (cond
    [(empty? env) Undefined-Error]
    [(equal? var (first (first env))) (second (first env))]
    [else (lookup var (rest env))]))

;; Examples for `lookup`
(check-equal? (lookup 'z '((x 42) (z 99))) 99) 

(define (extend-env var val env)
  (cons (list var val) env))

;; Examples for `extend-env`
(check-equal? (extend-env 'z 99 '((x 42))) '((z 99) (x 42)))

;; Define initial environment for arithmetic operations
(define initial-env
  (list (list '+ +)
        (list '- -)))

;; Error Checking Functions
(define (UNDEFINED-ERROR? r) (undefined-error? r))

;; Examples for `UNDEFINED-ERROR?`
(check-equal? (UNDEFINED-ERROR? Undefined-Error) #true) 

(define (NOT-FN-ERROR? r) (not-fn-error? r))

;; Examples for `NOT-FN-ERROR?`
(check-equal? (NOT-FN-ERROR? Not-Fn-Error) #true) 

;; Define custom syntax error for CS450Lang
(struct exn:fail:syntax:cs450 (msg origin cont-mark-set) #:transparent)

;; Constructor for exn:fail:syntax:cs450
(define (make-exn:fail:syntax:cs450 msg origin)
  (exn:fail:syntax:cs450 msg origin (current-continuation-marks)))

;; Examples for `make-exn:fail:syntax:cs450`
(check-equal? 
 (exn:fail:syntax:cs450? (make-exn:fail:syntax:cs450 "Test" 'parse)) 
 #true) 


;; parse : CS450LangExpr -> CS450LangAST
(define (parse expr)
  (match expr
    [(? number?) (num expr)]             
    [(? symbol?) (vari expr)]         
    [`(bind ,var ,e1 ,e2)                
     (bind-ast var (parse e1) (parse e2))]
    [`(,fn . ,args)                      
     (if (and (symbol? fn) (member fn '(+ - bind)))
         (call (vari fn) (map parse args))
         (raise (make-exn:fail:syntax:cs450 "Invalid function call" 'parse)))]
    [_ (raise (make-exn:fail:syntax:cs450 "Invalid expression" 'parse))])) 

;; Examples for `parse`
(check-equal? (parse '(+ 10 (+ 5 3))) 
              (call (vari '+) (list (num 10) (call (vari '+) (list (num 5) (num 3)))))) 


;; run : CS450LangAST Env -> CS450LangResult
(define (run ast env)
  (match ast
    [(num n) n]
    [(vari v) (lookup v env)]
    [(bind-ast var expr body)
     (let ([value (run expr env)])
       (run body (extend-env var value env)))]
    [(call fn args)
     (let ([fn-val (run fn env)]
           [arg-vals (map (λ (arg) (run arg env)) args)])
       (cond
         [(procedure? fn-val) (apply fn-val arg-vals)]
         [else Not-Fn-Error]))]
    [_ NaN]))

;; Examples for `run`
(check-equal? (run (call (vari '+) (list (num 10) (num 20))) initial-env) 30)

;; Examples for `initial-env`
(check-equal? (lookup '+ initial-env) +)