;№3
;Определите лисповскую форму (IF условие p q) в виде макроса.
(defmacro iff (f p q)(list 'if f p q))

(print (iff (> 2 3) 3 5))
(print (iff (< 2 3) 3 5))
