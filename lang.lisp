(defmacro x (k y)  
    `(
          cond
          ((equal '= ,k) (and ( setq x ,y) (print x)))
          ((equal + ,k) (and ( setq x (,k ,x ,y)) (print x)))
          ((equal '^ ,k) (and ( setq x (expt  ,x ,y)) (print x)))

      )
)
(setq x 0)
