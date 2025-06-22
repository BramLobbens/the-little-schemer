#lang racket

;; *(5. *Oh My Gawd*: It's Full of Stars)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;rember*
(define rember*
  (lambda (a lst)
    (cond
      [(null? lst) (quote ())]
      [(atom? (car lst))
       (cond
         [(eq? a (car lst)) (rember* a (cdr lst))]
         [else (cons (car lst) (rember* a (cdr lst)))])]
      [else (cons (rember* a (car lst)) (rember* a (cdr lst)))])))

(rember* 'cup (list (list 'coffee) 'cup (list (list 'tea) 'cup) (list 'and (list 'hick)) 'cup))