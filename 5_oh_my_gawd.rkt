#lang racket
(require "definitions.rkt")
(require "4_numbers_games.rkt")
;; *(5. *Oh My Gawd*: It's Full of Stars)

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
  (lambda (new old l)
    (cond
      [(null? l) (quote ())]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)
          (cons old (cons new (insertR* new old (cdr l))))]
         [else (cons (car l) (insertR* new old (cdr l)))])]
      [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))

(insertR* 'roast 'chuck
         (list (list 'how 'much (list 'wood)) 'could (list (list 'a (list 'wood) 'chuck))
              (list (list (list 'chuck))) (list 'if (list 'a) (list (list 'wood 'chuck)))
              'could 'chuck 'wood))

;;occur*
(define occur*
  (lambda (a lst)
    (cond
      [(null? lst) 0]
      [(atom? (car lst))
       (cond
         [(eq? a (car lst)) (add1 (occur* a (cdr lst)))]
         [else (occur* a (cdr lst))])]
      [else (+ (occur* a (car lst)) (occur* a (cdr lst)))]
      )))

(occur* 'banana (list (list 'banana) (list 'split (list (list (list (list 'banana 'ice)))
                                                       (list 'cream (list 'banana)) 'sherbet)) (list 'banana) (list 'bread) (list 'banana 'brandy)))

;;subst*
(define subst*
  (lambda (new old lst)
    (cond
      [(null? lst) (quote ())]
      [(atom? (car lst))
       (cond
         [(eq? old (car lst)) (cons new (subst* new old (cdr lst)))]
         [else (cons (car lst) (subst* new old (cdr lst)))])]
      [else (cons (subst* new old (car lst)) (subst* new old (cdr lst)))])))

(subst* 'orange 'banana (list (list 'banana) (list 'split (list (list (list (list 'banana 'ice)))
                                                               (list 'cream (list 'banana)) 'sherbet (list 'banana) (list 'bread) (list 'banana 'brandy)))))

;;insertL*
(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) (quote ())]
      [(atom? (car l))
       (cond
         [(eq? (car l) old)
          (cons new (cons old (insertL* new old (cdr l))))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))

(insertL* 'pecker 'chuck
         (list (list 'how 'much (list 'wood)) 'could (list (list 'a (list 'wood) 'chuck))
              (list (list (list 'chuck))) (list 'if (list 'a) (list (list 'wood 'chuck)))
              'could 'chuck 'wood))

;;member*
(define member*
  (lambda (a lst)
    (cond
      [(null? lst) #f]
      [(atom? (car lst))
       (cond
         [(eq? a (car lst)) #t]
         [else (member* a (cdr lst))])]
      [else (or (member* a (car lst)) (member* a (cdr lst)))])))

(member* 'chips
        (list (list 'potato) (list 'chips (list (list 'with) 'fish) (list 'chips))))

;;leftmost
(define leftmost
  (lambda (lst)
    (cond
      [(atom? (car lst)) (car lst)]
      [else (leftmost (car lst))])))

(eqan? 'hot (leftmost (list (list (list 'hot) (list 'tuna (list 'and))) 'cheese)))

;;eqlist?
;--version 1
(define eqlist?
  (lambda (l1 l2)
    (cond
      [(null? l1) (null? l2)] ;l1 is empty, l2 is empty then they are equal, if l2 not empty then false
      [(and (atom? (car l1)) (null? l2)) #f] ;l1 starts with an atom, l2 is empty
      [(and (null? l1) (atom? (car l2))) #f] ;l1 is empty, l2 starts with an atom
      [(and (atom? (car l1)) (atom? (car l2))) ;l1 and l2 start with an atom
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))] ;compare their values and recurse comparing the rest of l1 and l2
      [(or (atom? (car l1)) (atom? (car l2))) #f] ; if either l1 or l2 start with an atom still return false
      [else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

(eq? #t (eqlist? '() '()))
(eq? #f (eqlist? '() '(b)))
(eq? #f (eqlist? '(a) '()))
(eq? #t (eqlist? (list 'strawberry 'ice 'cream) (list 'strawberry 'ice 'cream)))
(eq? #f (eqlist? (list 'banana (list (list 'split))) (list (list 'banana) (list 'split))))
(eq? #f (eqlist? (list 'beef (list (list 'sausage)) (list 'and (list 'soda))) (list 'beef (list (list 'salami)) (list 'and (list 'soda)))))
(eq? #t (eqlist? (list 'beef (list (list 'sausage)) (list 'and (list 'soda))) (list 'beef (list (list 'sausage)) (list 'and (list 'soda)))))

;--version 2
(define eqlist2?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2))) #f]
      [else (and (eqlist2? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))])))

(eq? #t (eqlist2? '() '()))
(eq? #f (eqlist2? '() '(b)))
(eq? #f (eqlist2? '(a) '()))
(eq? #t (eqlist2? (list 'strawberry 'ice 'cream) (list 'strawberry 'ice 'cream)))
(eq? #f (eqlist2? (list 'banana (list (list 'split))) (list (list 'banana) (list 'split))))
(eq? #f (eqlist2? (list 'beef (list (list 'sausage)) (list 'and (list 'soda))) (list 'beef (list (list 'salami)) (list 'and (list 'soda)))))
(eq? #t (eqlist2? (list 'beef (list (list 'sausage)) (list 'and (list 'soda))) (list 'beef (list (list 'sausage)) (list 'and (list 'soda)))))

;;equal? -- mutually recurses over eqlist3?
(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2))
       (eqan? s1 s2)] ;helper function eqan?
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist3? s1 s2)])))

;--version 3
(define eqlist3?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t] ;both are the empty list
      [(or (null? l1) (null? l2)) #f] ;one list is empty other is not
      ;both are a non-empty list -- and we call the first s-exp of each list with equal? -- and recurse the rest of the lists
      [else (and (equal? (car l1) (car l2)) (eqlist3? (cdr l1) (cdr l2)))])))

(eq? #t (eqlist3? '() '()))
(eq? #f (eqlist3? '() '(b)))
(eq? #f (eqlist3? '(a) '()))
(eq? #t (eqlist3? (list 'strawberry 'ice 'cream) (list 'strawberry 'ice 'cream)))
(eq? #f (eqlist3? (list 'banana (list (list 'split))) (list (list 'banana) (list 'split))))
(eq? #f (eqlist3? (list 'beef (list (list 'sausage)) (list 'and (list 'soda))) (list 'beef (list (list 'salami)) (list 'and (list 'soda)))))
(eq? #t (eqlist3? (list 'beef (list (list 'sausage)) (list 'and (list 'soda))) (list 'beef (list (list 'sausage)) (list 'and (list 'soda)))))