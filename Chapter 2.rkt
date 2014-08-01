#lang racket

;;; Chapter 2

;;; Rational numbers data abstraction

(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (/ (* n d) (abs (* n d)))))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))

(define (numer num)
  (car num))

(define (denom num)
  (cdr num))

(define (print-rat num)
  (newline)
  (printf "~s/~s"
          (numer num)
          (denom num)))
          

(define (add-rat r1 r2)
  (make-rat (+ (* (numer r1) (denom r2))
               (* (denom r1) (numer r2)))
            (* (denom r1) (denom r2))))

(define (sub-rat r1 r2)
  (make-rat (- (* (numer r1) (denom r2))
               (* (denom r1) (numer r2)))
            (* (denom r1) (denom r2))))

(define (mul-rat r1 r2)
  (make-rat (* (numer r1) (numer r2))
            (* (denom r1) (denom r2))))

(define (div-rat r1 r2)
  (make-rat (* (numer r1) (denom r2))
            (* (denom r1) (numer r2))))

(define (equal-rat? r1 r2)
  (= (* (numer r1) (denom r2))
     (* (denom r2) (numer r2))))
