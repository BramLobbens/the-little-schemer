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

;;insertR*
(define insertR*
  (lambda (new_ old l)
    (cond
      [(null? l) (quote ())]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)
          (cons old (cons new_ (insertR* new_ old (cdr l))))]
         [else (cons (car l) (insertR* new_ old (cdr l)))])]
      [else (cons (insertR* new_ old (car l)) (insertR* new_ old (cdr l)))])))

(insertR* 'roast 'chuck
  (list (list 'how 'much (list 'wood)) 'could (list (list 'a (list 'wood) 'chuck)) (list (list (list 'chuck))) (list 'if (list 'a) (list (list 'wood 'chuck))) 'could 'chuck 'wood))