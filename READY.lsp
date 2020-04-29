
;№21 
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.
(defun del-elem(lst elem)
    (cond
      ((null lst) nil)
      (t
       ((lambda (x y)
        ((lambda (z)
         (cond
          ((null lst) nil)
          ((listp x)(cons (del-elem x elem) z))
          ((= elem x) y)
          (t(cons x z ))
         ))(del-elem y elem))
       )(car lst)(cdr lst))
     )
   )
)

(print(del-elem '(4 2 (4) 6 2 4 (2)) 4))
(print(del-elem '((9 3) 5 (3 (2 5) 3) 8 2 5 2 (3)) 2))


;№33
;Определите функцию МНОЖЕСТВО, преобразующую список в множество.
;функция объединение списков, аналог append
(defun app(lst1 lst2)
      (cond
         ((null lst1) lst2)
         (t(cons (car lst1) (app (cdr lst1) lst2)))
       )
)

;функция убирает вложеные списки (((a)) (b) c)->(a b c)
(defun list-line(lst)     
    (cond
      ((null lst) nil)
      (t
        ((lambda (x y)
          ((lambda (z)
            (cond
              ((null lst) nil)
              ((listp x)(app (list-line x) z ))
              (t(cons x z))
             )
            )(list-line y))
         )(car lst)(cdr lst)
        )
       )
     )
)

;функция убирает повторяющиеся элементы
(defun list-set(lst)
    (cond
      ((null lst) nil)
      (t
        ((lambda (x y)
          ((lambda (z)
            (cond
              ((null lst) nil)
              ((member x y)z)
              (t(cons x z))
            )
          )(list-set y))
        )(car lst)(cdr lst)) 
      )
     )
)

(print(list-set (list-line '( 1 2  3 4 5 6  7 8  9 8 7 6 5 4 3 2 1))))
(print(list-set (list-line '((3) 2 (8) ((7)) (3) 8))))


;№17
;Создайте предикат, порождающий всевозможные перестановки исходного множества.


(defun insert-elem-in-all-position (a l r)
  (cond 
      ((null r) (list (append l (list a))))
      (t
          (cons 
               (append l (list a) r) 
               (insert-elem-in-all-position a (append l (list (car r))) (cdr r))
           )
       )
   )
)
 

(defun add-elem-for-each-permutation (elem perm-lst)
	(cond
		((null perm-lst) nil)
		(t (append
				(insert-elem-in-all-position elem nil (car perm-lst))
				(add-elem-for-each-permutation elem (cdr perm-lst))))
	)
)

(defun my-permutation (lst)
	(cond
		((null lst) nil)
		((null (cdr lst)) (list lst))
		(t (add-elem-for-each-permutation
			(car lst)
			(my-permutation (cdr lst))))
	)
)

(print (my-permutation '(1 2 3)))
