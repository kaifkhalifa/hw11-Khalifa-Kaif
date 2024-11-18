#lang racket

(require rackunit)
(require "hw11.rkt")

;; Define the test suite
(define TESTS
  (test-suite
   "HW11 Tests"

   ;; Tests for CS450LangExpr?
   (check-equal? (CS450LangExpr? '(+ 1 2)) #true)
   (check-equal? (CS450LangExpr? '(bind x "hello" x)) #true)
   (check-equal? (CS450LangExpr? '(=== 3 3)) #true)
   (check-equal? (CS450LangExpr? '(FALSE '? 1 ': 0)) #true)

   ;; Tests for parse
   (check-equal? (parse '(+ 1 2)) (add (num 1) (num 2)))
   (check-equal? (parse '(bind y 3 (+ y 4))) 
                 (bind-ast 'y (num 3) (add (vari 'y) (num 4))))
   (check-equal? (parse '(=== "a" "b")) (equals (str "a") (str "b")))
   (check-equal? (parse '(FALSE '? "yes" ': "no")) 
                 (cond-expr (bool #false) (str "yes") (str "no")))

   ;; Tests for run
   (check-equal? (run (add (num 3) (num 5)) '()) 8)
   (check-equal? (run (bind-ast 'z (num 10) (vari 'z)) '()) 10)
   (check-equal? (run (call (vari '+) (list (num 4) (num 6))) 
                      (list (list '+ +))) 
                 10)
   (check-equal? (run (sub (num 10) (num 5)) '()) 5)

   ;; Tests for lookup
   (check-equal? (lookup 'a (list (list 'a 100))) 100)
   (check-equal? (lookup 'b (list (list 'a 100))) Undefined-Error)
   (check-equal? (lookup 'x (list (list 'x 42))) 42)

   ;; Tests for extend-env
   (check-equal? (extend-env 'a 1 '()) '((a 1)))
   (check-equal? (extend-env 'b 2 '((a 1))) '((b 2) (a 1)))

   ;; Tests for UNDEFINED-ERROR?
   (check-equal? (UNDEFINED-ERROR? Undefined-Error) #true)
   (check-equal? (UNDEFINED-ERROR? NaN) #false)

   ;; Tests for NOT-FN-ERROR?
   (check-equal? (NOT-FN-ERROR? Not-Fn-Error) #true)
   (check-equal? (NOT-FN-ERROR? NaN) #false)

   ;; Tests for exn:fail:syntax:cs450?
   (check-equal? 
    (exn:fail:syntax:cs450? 
     (make-exn:fail:syntax:cs450 "Error" 'test (current-continuation-marks))) 
    #true)
   (check-equal? 
    (exn:fail:syntax:cs450? 
     (make-exn:fail:syntax:cs450 "Another Error" 'test (current-continuation-marks))) 
    #true)
   ))

;; Run tests in verbose mode
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))

;; Provide the test suite
(provide TESTS)

