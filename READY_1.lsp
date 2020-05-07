
;№21 
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.
(defun del-elem(lst elem)
    (del lst elem nil)
)

(defun del(lst elem delet)
    (cond
        ((null lst)nil)
        (t
         ((lambda (x y)
          ((lambda (z)
           (cond
            ((null lst)nil)
            ((and (equal x elem) (null delet)) z)
            (t(cons x z))
          ))(del y elem delet))
         )(car lst)(cdr lst))
        )
    )
)

(print(del-elem '(4 2 (4) 6 2 4 (2)) 4))
(print(del-elem '((9 3) 5 (3 (2 5) 3) 8 2 5 2 (3)) 2))
(print (del-elem '(2 (4) 6 2 4 (2 4)) 4))


