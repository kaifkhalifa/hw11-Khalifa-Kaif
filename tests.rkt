#lang racket

(require rackunit)
(require "hw11.rkt")

;; Define the initial environment outside the test suite
(define init-env (list (list '+ +) (list '- -)))

;; Define the test suite
(define TESTS
  (test-suite
   "HW11 Tests"

   ;; Tests for CS450LangExpr?
   (check-equal? (CS450LangExpr? 42) #true)
   (check-equal? (CS450LangExpr? 'x) #true)
   (check-equal? (CS450LangExpr? '(bind x 1 x)) #true)
   (check-equal? (CS450LangExpr? '(+ 1 2)) #false)

   ;; Tests for parse
   (check-equal? (parse '(bind x 1 x)) (bind-ast 'x (num 1) (vari 'x)))
   (check-equal? (parse '(x . (2 3))) (call (vari 'x) (list (num 2) (num 3))))

   ;; Tests for lookup
   (check-equal? (lookup 'x (list (list 'x 5))) 5)
   (check-equal? (lookup 'y (list (list 'x 5))) Undefined-Error)

   ;; Tests for extend-env
   (check-equal? (extend-env 'x 5 '()) '((x 5)))
   (check-equal? (extend-env 'y 10 '((x 5))) '((y 10) (x 5)))

   ;; Tests for run
   (check-equal? (run (parse '(bind x 1 x)) init-env) 1)
   (check-equal? (run (parse '(bind x 1 (bind y 2 x))) init-env) 1)

   ;; Tests for apply-fn
   (check-equal? (apply-fn '+ '(1 2 3)) 6)
   (check-equal? (apply-fn '- '(10 5)) 5)
   (check-equal? (apply-fn 'not-a-fn '(1 2)) Not-Fn-Error)

   ;; Tests for custom-syntax-exception?
   (check-equal? (custom-syntax-exception? 
                  (exn:fail:syntax:cs450 "Invalid expression" 
                                         (current-continuation-marks) 
                                         'parse 'extra-info 'details)) 
                 #true)
   ))

;; Run tests in verbose mode
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))

;; Provide the test suite
(provide TESTS)
