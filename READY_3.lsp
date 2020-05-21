;№1
;Определите макрос, который возвращает свой вызов.
(defmacro self () `'(self))
(print (self))

;№2
;Определите макрос (POP стек), который читает из стека верхний элемент и
;меняет значение переменной стека.
(defmacro new_pop (lst)
     `( 
          let 
          ( ( temp ( car ,lst))) 
          ( setq ,lst (cdr ,lst)) 
          temp)
)

(setq steck (list 1 2 3 4))
(print steck)
(print (new_pop steck))
(print steck)
(print (new_pop steck))
(print steck)
(print (new_pop steck))

;№3
;Определите лисповскую форму (IF условие p q) в виде макроса.
(defmacro iff (f p q)`(if ,f ,p ,q))

(print (iff (> 2 3) 3 5))
(print (iff (< 2 3) 3 5))

;№4
;Определите в виде макроса форму (FIF тест отр нуль полож)
(defmacro fif (test negative zero positive)
     `(
          cond
          (( < ,test 0) ,negative)
          (( > ,test 0) ,positive)
          (,t ,zero)
      )
)

(print (fif -5 -1 0 1))
(print (fif 0 -1 0 1))
(print (fif 5 -1 0 1))

;№5
;Определите в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.
(defmacro repeat (e until p)
    `(
         cond
         ( ,p ( and ( print ,e) ( repeat ,e until ,p)))
         ( ,t ())
     )
)

(let ((i 0)) (repeat (incf i) until (< i 5)))
