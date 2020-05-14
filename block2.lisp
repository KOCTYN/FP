
;№1
;Определите FUNCALL через функционал APPLY.
(defun analog_funcall (f &rest args)
              (apply f args))

(print(analog_funcall '+  1 2 3 4))
(print(analog_funcall '*  6 2 3))

;№3
;Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.
(defun APL-APPLY(f x)
      (mapcar 'apply f x)
)

(print(APL-APPLY '(+ *)  '((1 2 3 4) (1 2 3 4))))

;№5
;Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, когда, 
;являющейся функциональным аргументом предикат пред истинен хотя бы для одного элемента списка список.
(defun certain (f lst)
     (not (null(mapcan 
                   (lambda (x)
                       (cond
                           ((funcall f x)(list t))
                           (t nil)
                       )
                    )lst)
          )
      )
)

(print(certain (lambda (x) (> x 5)) '(1 2 3 4 5 6 5)))
(print(certain (lambda (x) (> x 5)) '(1 2 3 4 5))) 

;№7
;Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
;все элементы, которые не обладают свойством, наличие которого проверяет предикат пред.
(defun delet (f lst)
     (mapcan 
                   (lambda (x)
                       (cond
                           ((funcall f x)(list x))
                           (t nil)
                       )
                    )lst)

)

(print(delet (lambda (x) (> x 5)) '(1 2 7 4 5 6 5)))
(print(delet (lambda (x) (= x 5)) '(1 2 5 3 4 5 6))) 

;№9
;Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...
 (defun fibonacci_numbers ()
     (let ( (x1 -1) (x2 1) (z 0))
         (lambda () (setq z (+ x1 x2)) (setq x1 x2) (setq x2 z) )
     )
 )
(setq c1 (fibonacci_numbers))

(setq c2 (fibonacci_numbers))

(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))

(print (funcall c2))
(print (funcall c2))
(print (funcall c2))
(print (funcall c2))
(print (funcall c2))

(print (funcall c1))
(print (funcall c1))
(print (funcall c1))

;№11
;Определите фукнционал МНОГОФУН, который использует функции, являющиеся
;аргументами, по следующей схеме:
;(МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).
(defun multifunctional (func lst)
    (mapcar (lambda (f) (apply f lst)) func)
)

(print (multifunctional '(+ - *) '(1 2 3 4)))

;№13
;Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).
(setq getme
    '(
        (lambda (x)
            (list x (list 'quote x))
        )
        '(lambda (x)
            (list x (list 'quote x))
        )
    )
)

(print getme)
(print (eval (eval (eval getme))))
