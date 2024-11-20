#lang racket

(require rackunit)
(require "hw11.rkt")

;; Define the test suite
(define TESTS
  (test-suite
   "HW11 Tests"

   ;; Tests for parse
   (check-equal? (parse '(bind x 1 x)) 
                 (bind-ast 'x (num 1) (vari 'x)))
   (check-equal? (parse '(+ 1 2)) 
                 (call (vari '+) (list (num 1) (num 2))))
   (check-equal? (parse '(- 10 5)) 
                 (call (vari '-) (list (num 10) (num 5))))
   (check-exn
    (λ (exn) (exn:fail:syntax:cs450? exn))  ; Validate the exception type
    (λ () (parse '(invalid 1 2))))          ; Call `parse` with invalid input


   ;; Tests for run
   (check-equal? (run (num 10) '()) 10)
   (check-equal? (run (bind-ast 'x (num 5) (vari 'x)) '()) 5)
   (check-equal? (run (call (vari '+) (list (num 4) (num 6))) initial-env) 10)
   (check-equal? (run (call (vari '-) (list (num 10) (num 3))) initial-env) 7)
   (check-equal? (run (vari 'y) '()) Undefined-Error)

   ;; Tests for lookup
   (check-equal? (lookup 'a (list (list 'a 100))) 100)
   (check-equal? (lookup 'b (list (list 'a 100))) Undefined-Error)

   ;; Tests for extend-env
   (check-equal? (extend-env 'a 1 '()) '((a 1)))
   (check-equal? (extend-env 'b 2 '((a 1))) '((b 2) (a 1)))

   ;; Tests for UNDEFINED-ERROR?
   (check-equal? (UNDEFINED-ERROR? Undefined-Error) #true)
   (check-equal? (UNDEFINED-ERROR? NaN) #false)

   ;; Tests for NOT-FN-ERROR?
   (check-equal? (NOT-FN-ERROR? Not-Fn-Error) #true)
   (check-equal? (NOT-FN-ERROR? NaN) #false)

   ;; Tests for initial-env
   (check-equal? (lookup '+ initial-env) +) 
   (check-equal? (lookup '- initial-env) -)
   ))

;; Run tests in verbose mode
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))

;; Provide the test suite
(provide TESTS)



