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

;;; 1.16 fast exponent calculation (logaritmic time) as iterative process

(define (fast-expt b n)
  (define (fe-step b n a)
    (cond ((= n 0) a)
          ((even? n) (fe-step (square b) (/ n 2) a))
          (else (fe-step b (- n 1) (* a b)))))
  (fe-step b n 1))

;;; 1.17 logarithmic recursive multiplication

(define (my-*-rec a b)
  ;; essentially binary shifts
  (define (double a)
    (* 2 a))
  (define (half a)
    (if (even? a) (/ a 2)
        'error))
  (cond ((= b 0) 0)
        ((even? b) (double (my-*-rec a (half b))))
        (else (+ a (my-*-rec a (- b 1))))))

;;; 1.18 logarithmic iterative multiplication

(define (my-*-it a b)
  (define (double a)
    (* 2 a))
  (define (half a)
    (if (even? a) (/ a 2)
        'error))
  (define (*-iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (*-iter (double a) (half b) acc))
          (else (*-iter a (- b 1) (+ acc a)))))
  (*-iter a b 0))
                     
;;; 1.19 logarithmic Fibonacci numbers

(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* q q) (* p p))
                     (+ (* q q) (* 2 q p))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))
