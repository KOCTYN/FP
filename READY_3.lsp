
;№2
;Определите макрос (POP стек), который читает из стека верхний элемент и
;меняет значение переменной стека.
(defmacro new_pop (lst)
     (list 
          'let 
          (list (list 'temp (list 'car lst))) 
          (list 'setq lst (list 'cdr lst)) 
          'temp)
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
(defmacro iff (f p q)(list 'if f p q))

(print (iff (> 2 3) 3 5))
(print (iff (< 2 3) 3 5))

;№4
;Определите в виде макроса форму (FIF тест отр нуль полож)
(defmacro fif (test negative zero positive)
     (list
          'cond
          (list (list '< test 0) negative)
          (list (list '> test 0) positive)
          (list t zero)
      )
)

(print (fif -5 -1 0 1))
(print (fif 0 -1 0 1))
(print (fif 5 -1 0 1))

;№5
;Определите в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.
(defmacro repeat (e until p)
    (list
         'cond
         (list p (list 'and (list 'print e) (list 'repeat e 'until p)))
         (list t ())
     )
)

(let ((i 0)) (repeat (incf i) until (< i 5)))
