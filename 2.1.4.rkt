#lang racket

;;; 2.1.4. Расширенный пример: интервальная арифметика
;; Лиза П. Хакер проектирует систему, которая помогала бы в решении технических
;; задач. Одна из возможностей, которые она хочет реализовать в своей системе, — способ-
;; ность работать с неточными величинами (например, измеренные параметры физических
;; устройств), обладающими известной погрешностью, так что когда с такими приблизи-
;; тельными величинами производятся вычисления, результаты также представляют собой
;; числа с известной погрешностью.

;; Идея Лизы состоит в том, чтобы реализовать «интервальную арифметику» как набор
;; арифметических операций над «интервалами» (объектами, которые представляют диа-
;; пазоны возможных значений неточной величины). Результатом сложения, вычитания,
;; умножения или деления двух интервалов также будет интервал, который представляет
;; диапазон возможных значений результата.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(define (make-interval a b) (cons a b))

;;; 2.7 selectors

(define (upper-bound int)
  (cdr int))

(define (lower-bound int)
  (car int))

;;; 2.8

(define (sub-interval x y)
  (let ((lo (- (lower-bound x)
               (upper-bound y)))
        (hi (- (upper-bound x)
               (lower-bound y))))
    (make-interval lo hi)))

;;; 2.12

(define (make-center-percent center percent)
  (let ((lower (- center (* (/ percent 100) center)))
        (upper (+ center (* (/ percent 100) center))))
    (make-interval lower upper)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* 100 (/ (width i) (center i))))