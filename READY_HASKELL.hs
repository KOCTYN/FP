-- №1
-- Определите функцию, возвращающую последний элемент списка

myLast [x] = x
myLast (_:xs) = myLast xs



-- №2
-- Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим
replace [] _  _  = []
replace  (x:xs) old new | (x == old) = new : (replace xs old new) 
                        | otherwise = x : (replace xs old new)



-- №12
-- Определите функцию, разбивающую список (a b с d...) на пары ((а b) (с d)...).
split [] = [] 
split [x] = [(x,x)] 
split (x1:x2:xs) = (x1,x2) : (split xs)


       
-- №18
-- Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух мно-
-- жеств (независимо от порядка следования элементов). Подсказка: напишите
-- функцию УДАЛИТЬ, удаляющую данный элемент из множества.


del_elem _ [] = []
del_elem v (x:xs) | (v == x) = del_elem v xs
                  | otherwise = x : (del_elem v xs)

check [] y = y
check (x:xs) y = (check xs (del_elem x y))

equal x y = if check x y == check y x then "equal" else "not equal"

-- №25 
-- Реализовать алгоритм быстрой сортировки.

quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, y >= x]


main = do 
       putStrLn "Task №1"
       print (myLast [1,2,3,4,5])
       
       putStrLn "Task №2"
       print (replace [1,2,3,4,2,5,2] 2 3)
       
       putStrLn "Task №12"
       print (split ["a","b","c","d","e","f","g"] ) 
       print (split [1,2,3,4,5] ) 
       
       putStrLn "Task №18"
       print (equal [5,4,3,2,1,2,3] [1,2,3,4,2,5,5,5]) 
       print (equal [1,2,3,4] [1,2,3,4,5]) 
       print (equal [1,2,3,4,5] [1,2,3,4])
       
       putStrLn "Task №25"
       print (quickSort [1,2,3,4,2,5,7,14,5,7,3,2,4,4,1])

 
