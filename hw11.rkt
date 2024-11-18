#lang racket

(provide (all-defined-out))

(require rackunit)

;; Data Definitions

;; A CS450LangExpr (Expr) is one of:
;; - Number
;; - Variable (Symbol)
;; - (list 'bind [Var Expr] Expr)
;; - (cons Expr List<Expr>)
;; Interpretation: Represents surface-level syntax of CS450Lang programs.

;; CS450LangExpr? : Any -> Boolean
;; Returns true if a value is a valid CS450LangExpr.
(define (CS450LangExpr? expr)
  (or (number? expr)
      (symbol? expr)  ;; Variables
      (and (list? expr)
           (match expr
             [`(bind ,var ,e1 ,e2)
              (and (symbol? var) (CS450LangExpr? e1) (CS450LangExpr? e2))]
             [`(,e1 . ,args)
              (and (CS450LangExpr? e1) 
                   (not (member e1 '(+ -))) 
                   (andmap CS450LangExpr? args))]
             [_ #false]))))
(check-equal? (CS450LangExpr? '(+ 1 2)) #false)

;; A CS450LangAST (AST) is one of:
;; - (num Number)
;; - (vari Symbol)
;; - (bind-ast Symbol AST AST)
;; - (call AST List<AST>)
;; Interpretation: Represents an abstract syntax tree for CS450Lang.

(struct num [val] #:transparent)
(struct vari [name] #:transparent)
(struct bind-ast [var expr body] #:transparent)
(struct call [fn args] #:transparent)
(check-equal? (num? (num 5)) #true)

;; A 450LangResult (Result) is one of:
;; - Number
;; - NaN
;; - ErrorResult

(struct nan [] #:transparent)
(struct undefined-error [] #:transparent)
(struct not-fn-error [] #:transparent)
(check-equal? (nan? (nan)) #true)

(define NaN (nan))
(define Undefined-Error (undefined-error))
(define Not-Fn-Error (not-fn-error))
(check-equal? Undefined-Error (undefined-error))

;; parse : CS450LangExpr -> CS450LangAST
(define/contract (parse expr)
  (-> CS450LangExpr? any/c)
  (match expr
    [(? number?) (num expr)]
    [(? symbol?) (vari expr)]
    [`(bind ,var ,e1 ,e2)
     (bind-ast var (parse e1) (parse e2))]
    [`(,e1 . ,args) (call (parse e1) (map parse args))]
    [_ (raise (exn:fail:syntax:cs450 "Invalid expression" (current-continuation-marks) 'parse))]))
(check-equal? (parse '(bind x 1 x)) (bind-ast 'x (num 1) (vari 'x)))

;; Environment Functions

;; lookup : Symbol Environment -> Result
(define (lookup var env)
  (cond
    [(empty? env) Undefined-Error]
    [(equal? var (first (first env))) (second (first env))]
    [else (lookup var (rest env))]))
(check-equal? (lookup 'x (list (list 'x 5))) 5)

;; extend-env : Symbol Result Environment -> Environment
(define (extend-env var val env)
  (cons (list var val) env))
(check-equal? (extend-env 'x 5 '()) '((x 5)))

;; Error Checking Functions
(struct exn:fail:syntax:cs450 exn:fail:syntax (origin additional-info extra-context))

(define (custom-syntax-exception? exn)
  (and (exn:fail:syntax:cs450? exn)
       (equal? (exn:fail:syntax:cs450-origin exn) 'parse)))

(check-equal? (custom-syntax-exception? 
               (exn:fail:syntax:cs450 "msg" 
                                      (current-continuation-marks) 
                                      'parse 
                                      'extra-info 
                                      'more-details)) 
              #true)


