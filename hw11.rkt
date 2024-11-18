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
;; - (list 'bind Var Expr Expr)
;; - (cons Expr List<Expr>)
;; Interpretation: Represents surface-level syntax of CS450Lang programs.

;; CS450LangExpr? : Any -> Boolean
;; Returns true if a value is a valid CS450LangExpr.
(define (CS450LangExpr? expr)
  (or (number? expr)
      (string? expr)
      (eq? expr 'TRUE)
      (eq? expr 'FALSE)
      (symbol? expr)  ;; Variable
      (and (list? expr)
           (match expr
             [`(+ ,x ,y) (and (CS450LangExpr? x) (CS450LangExpr? y))]
             [`(- ,x ,y) (and (CS450LangExpr? x) (CS450LangExpr? y))]
             [`(=== ,x ,y) (and (CS450LangExpr? x) (CS450LangExpr? y))]
             [`(,cond '? ,true-expr ': ,false-expr) 
              (and (CS450LangExpr? cond) 
                   (CS450LangExpr? true-expr) 
                   (CS450LangExpr? false-expr))]
             [`(bind ,var ,e1 ,e2) 
              (and (symbol? var) (CS450LangExpr? e1) (CS450LangExpr? e2))]
             [(list fn args ...) (and (CS450LangExpr? fn) (andmap CS450LangExpr? args))]
             [_ #false]))))
(check-equal? (CS450LangExpr? '(+ 1 2)) #true)

;; A CS450LangAST (AST) is one of:
;; - (num Number)
;; - (str String)
;; - (bool Boolean)
;; - (vari Symbol)
;; - (add AST AST)
;; - (sub AST AST)
;; - (equals AST AST)
;; - (cond-expr AST AST AST)
;; - (bind-ast Symbol AST AST)
;; - (call AST List<AST>)
;; Interpretation: Represents an abstract syntax tree for CS450Lang.

(struct num [val] #:transparent)
(struct str [val] #:transparent)
(struct bool [val] #:transparent)
(struct vari [name] #:transparent)
(struct add [left right] #:transparent)
(struct sub [left right] #:transparent)
(struct equals [left right] #:transparent)
(struct cond-expr [cond true-branch false-branch] #:transparent)
(struct bind-ast [var expr body] #:transparent)
(struct call [fn args] #:transparent)

;; A 450LangResult (Result) is one of:
;; - Number
;; - String
;; - Boolean
;; - NaN
;; - ErrorResult

(struct nan [] #:transparent)
(struct undefined-error [] #:transparent)
(struct not-fn-error [] #:transparent)

(define NaN (nan))
(define Undefined-Error (undefined-error))
(define Not-Fn-Error (not-fn-error))

;; Error Checking Functions
(struct exn:fail:syntax:cs450 (msg origin cont-mark-set) #:transparent)

;; Define the constructor manually
(define (make-exn:fail:syntax:cs450 msg origin cont-mark-set)
  (exn:fail:syntax:cs450 msg origin cont-mark-set))

(check-equal?
 (exn:fail:syntax:cs450? 
  (make-exn:fail:syntax:cs450 "Error message" 'test (current-continuation-marks)))
 #true)



(define (UNDEFINED-ERROR? r)
  (undefined-error? r))
(check-equal? (UNDEFINED-ERROR? Undefined-Error) #true)

(define (NOT-FN-ERROR? r)
  (not-fn-error? r))
(check-equal? (NOT-FN-ERROR? Not-Fn-Error) #true)

;; parse : CS450LangExpr -> CS450LangAST
(define/contract (parse expr)
  (-> CS450LangExpr? any/c)
  (match expr
    [(? number?) (num expr)]
    [(? string?) (str expr)]
    ['TRUE (bool #true)]
    ['FALSE (bool #false)]
    [(? symbol?) (vari expr)]
    [`(+ ,x ,y) (add (parse x) (parse y))]
    [`(- ,x ,y) (sub (parse x) (parse y))]
    [`(=== ,x ,y) (equals (parse x) (parse y))]
    [`(,cond '? ,true-expr ': ,false-expr) 
     (cond-expr (parse cond) (parse true-expr) (parse false-expr))]
    [`(bind ,var ,e1 ,e2) 
     (bind-ast var (parse e1) (parse e2))]
    [`(,fn . ,args) 
     (call (parse fn) (map parse args))]
    [_ (raise (exn:fail:syntax:cs450 "Invalid expression"
                                     (current-continuation-marks)
                                     'parse))]))
(check-equal? (parse '(bind x 1 x)) (bind-ast 'x (num 1) (vari 'x)))

;; Environment Functions

(define (lookup var env)
  (cond
    [(empty? env) Undefined-Error]
    [(equal? var (first (first env))) (second (first env))]
    [else (lookup var (rest env))]))
(check-equal? (lookup 'x (list (list 'x 5))) 5)

(define (extend-env var val env)
  (cons (list var val) env))
(check-equal? (extend-env 'x 5 '()) '((x 5)))

;; run : CS450LangAST Environment -> CS450LangResult
(define/contract (run ast env)
  (-> any/c list? any/c)
  (match ast
    [(num n) n]
    [(vari v) (lookup v env)]
    [(add x y) (+ (run x env) (run y env))]
    [(sub x y) (if (and (number? (run x env)) (number? (run y env)))
                   (- (run x env) (run y env))
                   NaN)]
    [(bind-ast var expr body)
     (let ([value (run expr env)])
       (run body (extend-env var value env)))]
    [(call fn args) 
     (let ([fn-val (run fn env)]
           [arg-vals (map (λ (arg) (run arg env)) args)])
       (match fn-val
         [`+ (apply + arg-vals)]
         [`- (apply - arg-vals)]
         [_ Not-Fn-Error]))]
    [_ NaN]))
(check-equal? (run (bind-ast 'x (num 5) (vari 'x)) '()) 5)
