#lang racket

;;; 1.7 quadratic newtonian interpolation
(define (square x)
  (* x x))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess)
                   x)))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0 x))

;;; 1.8 cubic interpolation

(define (cube x)
  (* x x x))

(define (root3 x)
  (define (root3-iter guess x)
    (if (good-enough-cube? guess x)
        guess
        (root3-iter (root3-improve guess x)
                    x)))
  (define (good-enough-cube? guess x)
    (< (abs (- (cube guess) x)) 0.001))
  (define (root3-improve guess x)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (root3-iter 1.0 x))
