#lang racket

;; (4. Numbers Games)

(provide eqan?)

;;=
(define eq-num
  (lambda (n m)
    (cond
      [(> n m) #f]
      [(< n m) #f]
      [else #t])))

;;exponent
(define my-expt
  (lambda (x y)
    (cond
      [(zero? y) 1]
      [else (* x (my-expt x (sub1 y)))])))

;;division
(define div
  (lambda (n m)
    (cond
      [(< n m) 0]
      [else (add1 (div (- n m) m))])))

;;length
(define len
  (lambda (lat)
    (cond
      [(null? lat) 0]
      [else (add1 (len (cdr lat)))])))

;;pick
(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)] ;;sub1 n because we're not 0-indexing;or could write equals n 1
      [else (pick (sub1 n) (cdr lat))])))

;;rempick (remove the pick from the list)
(define rempick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (cdr lat)]
      [else (cons (car lat)
                 (rempick (sub1 n) (cdr lat)))])))

;;no-nums
(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) null]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))])))

;;all-nums
(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) null]
      [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))])))

;;eqan? (any atom)
(define eqan?
  (lambda (a b)
    (cond
      [(and (number? a) (number? b)) (= a b)]
      [(number? a) (number? b)] ;or via 'or' -> '#f'
      [else (eq? a b)])))

;;occur
(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))

;;one?
; (define one?
;   (lambda (n)
;     (cond
;       [#t (= n 1)])))
(define one?
  (lambda (n) (= n 1)))

;;rempick2
(define rempick2
  (lambda (n lat)
    (cond
      [(one? n) (cdr lat)]
      [else (cons (car lat)
                 (rempick2 (sub1 n) (cdr lat)))])))