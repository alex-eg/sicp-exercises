#lang racket

;;; 1.7 quadratic newtonian interpolation
(define (square x)
  (* x x))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0))

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

;;; 1.29 Simpson integration

(define (sum a b term next)
  (if (> a b)
      0
      (+ (term a) (sum (next a) b term next))))

(define (sum-iterative a b term next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; simple
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum (+ a (/ dx 2)) b f add-dx)
     dx))

;; simpson
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))
 
  (* (/ h 3)
      (+ (yk 0) (yk n)
         (sum 1 (- n 1) (λ (k) (* 4 (yk k))) (λ (k) (+ k 2)))
         (sum 2 (- n 2) (λ (k) (* 2 (yk k))) (λ (k) (+ k 2))))))

;;; 1.32 Generic accumulation

(define (accumulate combiner null-value a b term next)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;;; 1.35 Golden ratio

(define (fixed-point f first-guess tolerance)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;; 1.36 Fixed point step counting

(define (fixed-point-steps f first-guess tolerance)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (display "guess: ")
    (display guess)
    (newline)
    (display "step: ")
    (display step)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next (+ 1 step)))))
  (try first-guess 0))

(define (golden-ratio)
  (fixed-point (λ (x) (+ 1 (/ 1 x))) 1.0 1e-6))

;;; 1.37 Continued fraction

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) ( + (d i) (rec (+ i 1))))))
  (rec 0))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 0)
        result
        (iter (- i 1) 
              (/ (n i) (+ (d i) result)))))
  (iter k 0))

;;; 1.38 Eulers function

(define (euler-e k)
  (cont-frac-iter (λ (x) 1.0)
                  (λ (x) (if (= (remainder x 3) 1)
                             (* (+ (quotient x 3) 1) 2.0)
                             1.0))
                  k))

(define (my-loop k d i)
    (cond 
      ((> k i)
       (display "i: ")
       (display i)
       (display " val: ")
       (display (d i))
       (newline)
       (my-loop k d (+ i 1)))))

;;; 1.39 Lambert's tangent

(define (tan-cf x k)
  (cont-frac-iter (λ (i) (if (= i 0) x (- (* x x))))
                  (λ (i) (+ (* 2 i) 1))
                  k))
;;; just useful function

(define (average a b)
  (/ (+ a b) 2))

;;; Newton's method

;;derivative computation
;; (x^3)' - 3x^2, x = 5 => (x^3)' = 75
;; ((deriv (lambda (x) (* x x x)) 0.00001) 5) => 75.00014999664018
(define (deriv g dx)
  (λ (x) (/ (- (g (+ x dx))
                    (g x))
                 dx)))

(define (newton-transform g dx)
  (λ (x)
    (- x (/ (g x) ((deriv g dx) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g 0.00001) guess 0.001))

(define (newtons-sqrt x)
  (newtons-method (λ (y) (- (* y y) x))
                  1.0))

;;; 1.40 cubic equations

(define (cubic a b c)
  (λ (x)
    (+ (* x x x) (* a x x) (* b x) c)))

(define (newtons-cubic a b c)
  (newtons-method (cubic a b c) 1.0))
    
;;; 1.41 double

(define (double fun)
  (λ (x) (fun (fun x))))

;;; 1.42 compose

(define (compose f g)
  (λ (x) (f (g x))))


;;; 1.43 repeated application

(define (repeated f n)
  (if (<= n 1)
      f
      (compose f (repeated f (- n 1)))))

;;; 1.44 function smoothing

(define (smooth f dx)
  (λ (x) (/ (+ (f (- x dx))
               (f x)
               (f (+ x dx)))
            3)))

(define (n-smooth f dx n)
  ((repeated smooth n) f dx))

;;; 1.46 iterative improvement

(define (iterate-improve good-enough? improve)
  (define (improve-step guess)
    (if (good-enough? guess)
        guess
        (improve-step (improve guess))))
  improve-step)

(define (sqrt-new x)
  ((iterate-improve
   (λ (guess) (< (abs (- (square guess) x)) 0.001))
   (λ (guess) (/ (+ guess (/ x guess)) 2.0)))
   x))

(define (fixed-point-new f first-guess tolerance)
  ((iterate-improve 
    (λ (guess) (< (abs (- guess (f guess)))
                  tolerance))
    (λ (guess) (f guess)))
   first-guess))