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

-- деревья №2
-- Определите функцию, находящую максимальное из значений, находящихся в вершинах дерева

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)              
              
mytree = Branch 3 (Branch 6 Empty Empty) (Branch 9 (Branch 8 Empty Empty) (Branch 2 Empty Empty))

treeInsert (Branch x Empty Empty) maxx  | (x>maxx) = x
                                        | otherwise = maxx
                                          
treeInsert (Branch x left right) maxx | (x>maxx) = max (treeInsert left x) (treeInsert right x)
                                      | otherwise = max (treeInsert left maxx) (treeInsert right maxx)

-- №2.3 Реализовать энкодер и декодер для кода Хемминга способного исправлять одну ошибку.     
encode x = control_bits (encoder [0,1,0,0,0,1,0,0,0,0,1,1,1,1,0,1] 1) 1     
       
encoder [] y    =  []
encoder (x:xs) n | ((fromIntegral(truncate (logBase 2 n)))==(logBase 2 n)) = 0 : (encoder (x:xs) (n+1))
                 | otherwise = x : (encoder xs (n+1))   
                   
control_bits [] _ = []         
control_bits (x:xs) n   | ((fromIntegral(truncate (logBase 2 n)))==(logBase 2 n)) = (mod (funcc (0:xs) 1 0 1 1 n) 2) : control_bits xs (n+1)
                        | otherwise = x : control_bits xs (n+1)

funcc [] _ k _ _ _ =  k
funcc (x:xs) n k nomer step col    | ((x==1) && (nomer==col) && (step == 1)) = funcc xs (n+1) (k+1) (nomer-1) 0 col
                                   | ((x==1) && (nomer/=col) && (step == 1)) = funcc xs (n+1) (k+1) (nomer+1) 1 col
                                   | ((x==0) && (nomer==col) && (step == 1)) = funcc xs (n+1) k (nomer-1) 0 col
                                   | ((x==0) && (nomer/=col) && (step == 1)) = funcc xs (n+1) k (nomer+1) 1 col
                                   
                                   | ((x==1) && (nomer==0) && (step == 0)) = funcc xs (n+1) k (nomer+1) 1 col
                                   | ((x==0) && (nomer==0) && (step == 0)) = funcc xs (n+1) k (nomer+1) 1 col
                                   | ((x==1) && (nomer/=0) && (step == 0)) = funcc xs (n+1) k (nomer-1) 0 col
                                   | ((x==0) && (nomer/=0) && (step == 0)) = funcc xs (n+1) k (nomer-1) 0 col
  
decode x = decoder(correction_mistake x (check_mistake (control_bits x 1) x 0 1) 1) 1

decoder [] _ = []
decoder (x:xs) n | ((fromIntegral(truncate (logBase 2 n)))==(logBase 2 n)) = decoder xs (n+1)
             | otherwise = x : decoder xs (n+1)

correction_mistake [] _ _ = []
correction_mistake (x:xs) k n | (k==n) = 0  : correction_mistake xs k (n+1)
                   | otherwise = x : correction_mistake xs k (n+1)

check_mistake [] [] k _ = k
check_mistake (x:xs) (y:ys) k n | (((fromIntegral(truncate (logBase 2 n)))==(logBase 2 n)) && (x/=y)) = check_mistake xs ys (k+n) (n+1)
                                 | otherwise = check_mistake xs ys k (n+1)

message = [0,1,0,0,0,1,0,0,0,0,1,1,1,1,0,1]

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
       
       putStrLn "Task №3.2"
       print (treeInsert mytree 0)
       
       putStrLn "Task №2.3"
       putStrLn "message"
       print message
       putStrLn "message_encode"
       let message_encode = encode message
       print message_encode
       putStrLn "message_decode"
       let message_decode = decode message_encode
       print message_decode
       putStrLn "message_decode_mistake_symbol"
       let message_decode_mistake_symbol = decode [1,0,0,1,1,0,0,0,1,1,0,0,0,0,1,0,1,1,1,0,1]
       print message_decode_mistake_symbol

