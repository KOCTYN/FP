;42
;Определите функцию, находящую максимальное из значений, находящихся в вершинах дерева.
(defun tree(lst)
         ((lambda (x l r)
          (if (and (null l) (null r))
             x         
             (max (max (tree l) x)  (tree r))
          )
         )(car lst)(cadr lst)(caddr lst)
))

(print(tree '(3 (4 nil nil)(5(3 nil nil)(2 nil nil)))))
(print(tree '(5 (3 (1 nil nil)(4 nil nil))(7(6 nil nil)(13(11 nil nil)(15 nil nil)))) ))


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
         (t(STRING= (get x1 'father) (get x2 'father)))
         (t(STRING= (get x1 'mother) (get x2 'mother)))
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
          ((atom (car lst))(check-list(cdr lst)))
          (t())
       )
)
(print(check-list '((2))))
(print(check-list '(2 (8) ((7)))))
(print(check-list '(2 8 7)))

;№21 
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.
(defun del-elem(lst elem)
    ((lambda (x y)
      (cond
         ((null lst) nil)
          ((= elem x) y)
          (t(cons x (del-elem y elem)))
       )
     )(car lst)(cdr lst)
))

(print(del-elem '(4 2 6 2 4 ) 4))
(print(del-elem '(8 2 5 2 7 9 4 2) 2))


;№33
;Определите функцию МНОЖЕСТВО, преобразующую список в множество.
(defun app(lst1 lst2)
      (cond
         ((null lst1) lst2)
         (t(cons (car lst1) (app (cdr lst1) lst2)))
       )
)

(defun list-line(lst)
    ((lambda (x y)
      (cond
         ((null lst) nil)
          ((listp x)(app (list-line x) (list-line y) ))
          (t(cons x (list-line y)))
       )
     )(car lst)(cdr lst)
))

(defun list-set(lst)
    ((lambda (x y)
      (cond
         ((null lst) nil)
          ((member x y)(list-set y))
          (t(cons x (list-set y)))
       )
    )(car lst)(cdr lst)
))

(print(list-set (list-line '( 1 2  3 4 5 6  7 8  9 8 7 6 5 4 3 2 1))))
(print(list-set (list-line '((3) 2 (8) ((7)) (3) 8))))

;№17
;Создайте предикат, порождающий всевозможные перестановки исходного множества.
(defun insert-elem-in-each-position (elem list)
	(cond
		((null list) (list elem))
		((atom list) (insert-elem-in-each-position elem (list list)))
		(t (cons (cons elem list)
			     (insert-elem-in-each-position-aux elem nil list)))
	)
)

(defun insert-elem-in-each-position-aux (elem list1 list2)
	(cond
		((null list2) nil)
		(t
			((lambda (a)
				(cons
					(append (car a) (list elem) (cadr a))
					((lambda (x)
						(insert-elem-in-each-position-aux elem
							(first x)
							(second x)))
					a))
			)
			((lambda (list1 list2)
				(list (append list1 (list (car list2))) (cdr list2)))
			list1 list2)))
		)
	)

(defun add-elem-for-each-permutation (elem perm-lst)
	(cond
		((null perm-lst) nil)
		(t (append
				(insert-elem-in-each-position elem (car perm-lst))
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


;------------------------------------------------ПРИНЯТЫЕ-----------------------------------------------
                
;№6
;Определите функцию, переводящую список чисел в список соответствующих им названий.
(defun number-string(lst)
      (cond
         ((null lst) nil)
         ((= (car lst) 1)"один")
         ((= (car lst) 2)"два")
         ((= (car lst) 3)"три")
         ((= (car lst) 4)"четыре")
         ((= (car lst) 5)"пять")
         ((= (car lst) 5)"пять")
         ((= (car lst) 6)"шесть")
         ((= (car lst) 7)"семь")
         ((= (car lst) 8)"восемь")
         ((= (car lst) 9)"девять")
         ((= (car lst) 10)"десять")
         ((= (car lst) 11)"одинадцать")
         ((= (car lst) 12)"двенадцать")
         ((= (car lst) 13)"тринадцать")
         ((= (car lst) 14)"четырнадцать")
         ((= (car lst) 15)"пятнадцать")
         ((= (car lst) 16)"шестнадцать")
         ((= (car lst) 17)"семнадцать")
         ((= (car lst) 18)"восемнадцать")
         ((= (car lst) 19)"девятнадцать")
         ((= (car lst) 20)"двадцать")
         ((= (car lst) 30)"тридцать")
         ((= (car lst) 40)"сорок")
         ((= (car lst) 50)"пятьдесят")
         ((= (car lst) 60)"шестьдесят")
         ((= (car lst) 70)"семьдесят")
         ((= (car lst) 80)"восемьдесят")
         ((= (car lst) 90)"девяносто")
         ((= (car lst) 100)"сто")
         ((= (car lst) 200)"двести")
         ((= (car lst) 300)"триста")
         ((= (car lst) 400)"четыреста")
         ((= (car lst) 500)"пятсот")
         ((= (car lst) 600)"шестьсто")
         ((= (car lst) 700)"семьсот")
         ((= (car lst) 800)"восемьсот")
         ((= (car lst) 900)"девяться")
         ((= (car lst) 1000)"тысяча")  

         ((> (car lst) 100)((lambda (y) (concatenate 'string (number-string(cons (- (car lst) y) ())) " " (number-string(cons y ())))) (rem (car lst) (expt 10 (number-length (car lst) 0) )) ))
         (t((lambda (x) (concatenate 'string (number-string(cons (- (car lst) x) ())) " " (number-string(cons x () )) ))(rem (car lst) 10)))

       )
)
(defun numberr(lst)
      (cond
         ((null lst) nil)
         (t(cons (number-string (cons (car lst)())) (numberr (cdr lst))))
       )
)

(defun number-length(lst n)
      (cond
          ((<= lst 0) (- n 1))
         (t(number-length (/ (- lst (rem lst 10)) 10) (+ 1 n)))
       )
)
(print(numberr '(1452 1231 300 755 836 256 746 216 456 121 19 16 20 22 75 38 74 31 62 1 2 3)))

;№15
;Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.
(defun cross-prod(v1 v2)
      (cond
          ((null v1) 0)
          (t (+ (* (car v1) (car v2)) (cross-prod(cdr v1) (cdr v2))))
       )
)
(print(cross-prod '(2 3 5) '(3 7 9)))

;№9
;Определите функцию, разделяющую исходный список на два подсписка. 
;В первый из них должны попасть элементы с нечетными номерами, во второй — элементы с четными номерами.
(defun split(lst)
      (cond
         ((null lst) '(() ()))
         (t((lambda (x y z)
               (cons (cons x (car z)) 
                  (cons      
                      (cond
                         ((null y) (cadr z))
                         (t(cons y (cadr z))))
                  ())
               )
            ) (car lst) (cadr lst) (split(cddr lst)))
          )
       )
)

(print(split '( 10 2 4 2)))
(print(split '( 9 7 3 )))
(print(split '( 1  3 2 3 2 2 3 4 5)))
(print(split '( 10 10 10 9 7 2 3 4 9 4 8 )))


;№22
;Определите функцию, которая обращает список (а b с) и разбивает его на уровни (((с) b) а).
(defun level-list(lst)
      (cond
         ((null(cdr  lst)) (cons (car lst) ()))
         (t(list (level-list (cdr lst)) (car lst) ))
       )
)
(print(level-list '( 1 2  3 4 5 6  7 8  9 )))                
(print(level-list '( 1  2 )))

