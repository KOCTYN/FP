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

;№9
;Определите функцию, разделяющую исходный список на два подсписка. 
;В первый из них должны попасть элементы с нечетными номерами, во второй — элементы с четными номерами.
(defun sub-lists(lst)
      (cond
         ((null lst) '(() ()))
         ( (evenp (car lst)) (cons (car (sub-lists(cdr lst))) (cons (cons (car lst)   (cadr (sub-lists(cdr lst))))())))
         (t(cons (cons (car lst) (car (sub-lists(cdr lst))))   (cdr (sub-lists(cdr lst)))))
       )
)
(print(sub-lists '( 1 2  3 4 5 6  7 8  9 )))
(print(sub-lists '( 10 2 4 )))
(print(sub-lists '( 9 7 3 )))
(print(sub-lists '( 10  9 7 2 3 4 9 4 8 )))

;№15
;Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.
(defun cross-prod(v1 v2)
      (cond
          ((null v1) 0)
          (t (+ (* (car v1) (car v2)) (cross-prod(cdr v1) (cdr v2))))
       )
)
(print(cross-prod '(2 3 5) '(3 7 9)))

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
(defun del-elem(lst)
      (cond
         ((null lst)  nil)
          (t(cdr lst))
       )
)
(print(del-elem '(2 8 5 (3))))

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

;№33
;Определите функцию МНОЖЕСТВО, преобразующую список в множество.
(defun list-line(lst)
      (cond
         ((null lst) nil)
          ((listp (car lst))(append (list-line(car lst)) (list-line(cdr lst)) ))
          (t(cons (car lst) (list-line(cdr lst))))
       )
)

(defun list-set(lst)
      (cond
         ((null lst) nil)
          ((member (car lst) (cdr lst))(list-set(cdr lst)))
          (t(cons (car lst) (list-set(cdr lst))))
       )
)

(print(list-set (list-line '( 1 2  3 4 5 6  7 8  9 8 7 6 5 4 3 2 1))))
(print(list-set (list-line '((3) 2 (8) ((7)) (3) 8))))
                
