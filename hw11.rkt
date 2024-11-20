#lang racket

(provide (all-defined-out))

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

;; Environment Functions
(define (lookup var env)
  (cond
    [(empty? env) Undefined-Error]
    [(equal? var (first (first env))) (second (first env))]
    [else (lookup var (rest env))]))

(define (extend-env var val env)
  (cons (list var val) env))

;; Define initial environment for arithmetic operations
(define initial-env
  (list (list '+ +)
        (list '- -)))

;; Error Checking Functions
(define (UNDEFINED-ERROR? r) (undefined-error? r))
(define (NOT-FN-ERROR? r) (not-fn-error? r))

;; Define custom syntax error for CS450Lang
(struct exn:fail:syntax:cs450 (msg origin cont-mark-set) #:transparent)

;; Constructor for exn:fail:syntax:cs450
(define (make-exn:fail:syntax:cs450 msg origin)
  (exn:fail:syntax:cs450 msg origin (current-continuation-marks)))


;; parse : CS450LangExpr -> CS450LangAST
(define (parse expr)
  (match expr
    [(? number?) (num expr)]              ; Parse numbers
    [(? symbol?) (vari expr)]             ; Parse variables
    [`(bind ,var ,e1 ,e2)                 ; Parse `bind`
     (bind-ast var (parse e1) (parse e2))]
    [`(,fn . ,args)                       ; Parse function calls
     (if (and (symbol? fn) (member fn '(+ - bind)))
         (call (vari fn) (map parse args))
         (raise (make-exn:fail:syntax:cs450 "Invalid function call" 'parse)))]
    [_ (raise (make-exn:fail:syntax:cs450 "Invalid expression" 'parse))])) ; Catch-all for invalid expressions


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

;; Examples 
(check-equal? (parse '(bind x 1 x)) (bind-ast 'x (num 1) (vari 'x)))
(check-equal? (lookup 'x (list (list 'x 5))) 5)
(check-equal? (extend-env 'x 5 '()) '((x 5)))
(check-equal? (run (bind-ast 'x (num 5) (vari 'x)) '()) 5)
(check-equal? (run (call (vari '+) (list (num 4) (num 6))) initial-env) 10)
