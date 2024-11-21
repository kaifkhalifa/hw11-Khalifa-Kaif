#lang racket

(require rackunit)
(require "hw11.rkt")

;; Define the test suite
(define TESTS
  (test-suite
   "HW11 Tests"


   ;; Tests for `lookup`

   (check-equal? (lookup 'z '((x 42) (z 99))) 99)
   (check-equal? (lookup 'x '((x 42) (z 99))) 42)
   ;; Additional Test
   (check-equal? (lookup 'w '((x 42) (z 99))) Undefined-Error)


   ;; Tests for `extend-env`

   (check-equal? (extend-env 'z 99 '((x 42))) '((z 99) (x 42)))
   ;; Additional Test
   (check-equal? (extend-env 'a 100 '()) '((a 100)))


   ;; Tests for `parse`

   (check-equal? (parse '(+ 10 (+ 5 3))) 
                 (call (vari '+) (list (num 10) (call (vari '+) (list (num 5) (num 3))))))
   (check-equal? (parse 'z) (vari 'z))
   ;; Additional Test
   (check-equal? (parse '(bind x 5 (+ x 2))) 
                 (bind-ast 'x (num 5) (call (vari '+) (list (vari 'x) (num 2)))))


   ;; Tests for `run`

   (check-equal? (run (call (vari '+) (list (num 10) (num 20))) initial-env) 30)
   (check-equal? 
    (run (bind-ast 'x (num 5) (bind-ast 'y (num 10) (call (vari '+) (list (vari 'x) (vari 'y))))) '()) 
    15)
   (check-equal? (run (vari 'z) '()) Undefined-Error)
   ;; Additional Test
   (check-equal?
    (run (bind-ast 'x (num 5) (bind-ast 'y (call (vari '+) (list (vari 'x) (num 3))) (vari 'y))) '())
    8)

   ;; Tests for Variadic Addition in `run`

   (check-equal? (run (call (vari '+) (list (num 1) (num 2) (num 3))) initial-env) 6)
   ;; Additional Test
   (check-equal? (run (call (vari '+) (list (num 4) (num 5) (num 6) (num 7))) initial-env) 22)



   ;; Tests for Shadowing

   (check-equal? (run (bind-ast 'x (num 5) (bind-ast 'x (num 10) (vari 'x))) '()) 10)
   ;; Additional Test
   (check-equal? 
    (run (bind-ast 'x (num 5) (bind-ast 'x (num 10) (bind-ast 'x (num 15) (vari 'x)))) '()) 15)


   ;; Tests for Environment in Nested Bindings

   (check-equal?
    (run (bind-ast 'x (num 5) (bind-ast 'y (call (vari '+) (list (vari 'x) (num 3))) (vari 'y))) '())
    8)
   ;; Additional Test
   (check-equal?
    (run (bind-ast 'x (num 5) (bind-ast 'y (num 10) (call (vari '+) (list (vari 'x) (vari 'y))))) '())
    15)


   ;; Tests for `initial-env`

   (check-equal? (lookup '+ initial-env) +)
   ;; Additional Test
   (check-equal? (lookup '- initial-env) -)
   ))

;; Run tests in verbose mode
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))

;; Provide the test suite
(provide TESTS)
