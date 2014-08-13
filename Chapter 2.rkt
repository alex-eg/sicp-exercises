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


;;; 2.2 point data

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment))
                    (x-point (end-segment segment)))
                 2)
              (/ (+ (y-point (start-segment segment))
                    (y-point (end-segment segment)))
                 2)))

;;; 2.3 rectangles

(define (make-rectangle tl br)
  (cons tl br))

(define (rect-top-left rect)
  (car rect))

(define (rect-bottom-right rect)
  (cdr rect))


;;; 2.4 Functional pairs

(define (ch-cons x y)
  (λ (m) (m x y)))

(define (ch-car z)
  (z (λ (p q) p)))

(define (ch-cdr z)
  (z (λ (p q) q)))

;;; 2.6 Church numbers

(define (zero)
  (λ (f) (λ (x) x)))

(define (add-1 n)
  (λ (f) (λ (x) (f ((n f) x)))))

(define (one)
  (λ (f) (λ (x) (f x))))

(define (two)
  (λ (f) (λ (x) (f (f x)))))

;;; 2.17

(define (last-pair list)
  (if (null? (cdr list)) list
      (last-pair (cdr list))))

;;; 2.18

(define (reverse list)
  (define (reverse-it list acc)
    (if (null? list) acc
        (reverse-it (cdr list) (cons (car list) acc))))
  (reverse-it list null))

;;; 2.20

(define (same-parity f . s)
  (define (par-it list acc)
    (if (null? list) acc
        (if (= (remainder (car list) 2)
               (remainder f 2))
            (par-it (cdr list) (cons (car list ...