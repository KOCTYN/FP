;№47
;Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.
( setf ( get 'x 'a) 1)
( setf ( get 'x 'b) 5)
( setf ( get 'x 'c) 9)
( setf ( get 'x 'd) 3)
( setf ( get 'x 'e) -25)
( setf ( get 'x 'f) 9999)
( setf ( get 'x 'g) 0.1)

(defun УДАЛИТЬ-ВСЕ-СВОЙСТВА(x)
    ((lambda (y)
      (cond
         ((null y)())
         (t(remprop x (car y))(УДАЛИТЬ-ВСЕ-СВОЙСТВА x))
       )
      )(symbol-plist 'x))
)

(print(symbol-plist 'x))
(print(УДАЛИТЬ-ВСЕ-СВОЙСТВА 'x))
(print(symbol-plist 'x))

;№46
;Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо. 
;Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и предикат (СЕСТРЫ-БРАТЬЯ x1 x2),
;который истинен в случае, если x1 и x2 — сестры или братья, родные или с одним общим родителем.
( setf ( get 'C 'mother) 'A)
( setf ( get 'C 'father) 'B)

( setf ( get 'H 'mother) 'E)
( setf ( get 'H 'father) 'R)

( setf ( get 'K 'mother) 'M)
( setf ( get 'K 'father) 'N)

( setf ( get 'Q 'mother) 'M)
( setf ( get 'Q 'father) 'N)

(defun get-mother(x)
(get x 'mother)
)

(defun get-father(x)
(get x 'father)
)

(defun parents(x)
(list (get-mother x) (get-father x))
)

(defun sisters-brothers(x1 x2)
    (cond
        ((STRING= (get x1 'father) (get x2 'father))t)
        ((STRING= (get x1 'mother) (get x2 'mother))t)
        (t nil)
    )
)

; A  B      E  R      M  N
;  \/        \/        \/
;  С         H        K  Q


(print(parents 'C))
(print(parents 'H))
(print(parents 'Q))
(print(parents 'K))

(print(sisters-brothers 'H 'K))
(print(sisters-brothers 'K 'Q))

;№18
;Определите предикат, проверяющий, является ли аргумент одноуровневым списком.
(defun check-list(lst)
      (cond
         ((null lst) t)
          ((listp (car lst)) NIL)
          (t(check-list(cdr lst)))
       )
)
(print(check-list '((2))))
(print(check-list '(2 (8) ((7)))))
(print(check-list '(2 8 7)))


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

(print(del-elem '(4 2 (4) 6 2 4 (2)) '4))
(print(del-elem '((9 3) 5 (3 (2 5) 3) 8 2 5 2 (3)) '2))


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
