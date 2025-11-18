{-# LANGUAGE EmptyDataDeriving #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Организационни въпроси:
    -- Другия вторник не се учи. За кога да изместим часа?
    -- 09.12 няма да имаме час. За кога да го изместим?
    -- Кога да направим защити на домашните?

-- Дървета

-- Задача 1: Да се дефинира тип за бинарно дърво
data BTree = Empty | Node Int BTree BTree

-- Задача 2: Да се дефинира тип за дърво
data Tree = Empty1 | Node1 Int [Tree]

-- Това не го гледайте:
-- stackoverflow.com/questions/12556469/nicely-printing-showing-a-binary-tree-in-haskell
instance Show BTree where
   show :: BTree -> String
   show Empty = "Empty"
   show (Node n t1 t2) = unlines (prettyprintHelper (Node n t1 t2))
       where 
           prettyprintHelper :: BTree -> [[Char]]
           prettyprintHelper (Node node left right)
               = show node : prettyprint_subtree left right
                   where
                       prettyprint_subtree l r =
                           pad "+- " "|  " (prettyprintHelper r)
                               ++ pad "`- " "   " (prettyprintHelper l)
                       pad first rest = zipWith (++) (first : repeat rest)
           prettyprintHelper Empty = []



-- За следващите задачи дърво ще означава бинарно дърво
tree :: BTree
tree = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 22 (Node 33 Empty Empty) (Node 44 Empty Empty))

-- Задача 3: Да се напише функция, която връща броя на елементите на дърво, които удовлетворяват даден предикат
elemCount :: BTree -> Int
elemCount Empty = 0
elemCount (Node _ t1 t2) = 1 + elemCount t1 + elemCount t2

elemCount2 :: (Int -> Bool) -> BTree -> Int
elemCount2 _ Empty = 0
elemCount2 f (Node v t1 t2) = if f v then 1 + elemCount2 f t1 + elemCount2 f t2 else elemCount2 f t1 + elemCount2 f t2

-- Задача 4: Да се напише функция, която приема бинарно дърво и връща сумата на всичките му листа
leafSum :: BTree -> Int
leafSum Empty = 0
leafSum (Node v Empty Empty) = v
leafSum (Node _ l r) = leafSum l + leafSum r

-- Задача 5: Да се напише функция, която намира най-големия по стойност елемент на дърво. 
    -- Приемете, че всички стойности са положителни
maxElem :: BTree -> Int
maxElem Empty = 0
maxElem (Node v l r) = maximum [v, maxElem l, maxElem r]

-- Задача 6: Да се напише функция, която приема списък и връща BST
createBST :: [Int] -> BTree
createBST [] = Empty 
createBST (x : xs) = Node x (createBST $ filter (<x) xs) (createBST $ filter (>=x) xs)

-- Отговор на въпрос: (Да се напише функция, която приема списък и връща дърво)
-- [1,2,3,4,5,6] ->    1
--                 2       3
--               4   5   6

createTree :: [Int] -> BTree
createTree l = helper l 0
    where 
        helper :: [Int] -> Int -> BTree
        helper [] _ = Empty
        helper xs i
         | i >= length xs = Empty
         | otherwise = Node (xs !! i) (helper xs ((i+1) * 2 - 1)) (helper xs ((i + 1) * 2))

-- Задача 7: Да се дефинира функция isOrdered, проверява дали дадено дърво е двоично наредено.

-- Вариант 1
isOrdered :: BTree -> Bool
isOrdered Empty = True
isOrdered (Node _ Empty Empty) = True
isOrdered (Node v l@(Node x _ _) Empty) = x < v && isOrdered l
isOrdered (Node v Empty r@(Node y _ _)) = y >= v && isOrdered r
isOrdered (Node v l@(Node x _ _ ) r@(Node y _ _)) = x < v && y >= v && isOrdered l && isOrdered r

-- Вариант 2
isOrdered2 :: BTree -> Bool
isOrdered2 Empty = True
isOrdered2 (Node v l r) = check l (<v) && check r (>=v) && isOrdered l && isOrdered r
    where 
        check :: BTree -> (Int -> Bool) -> Bool
        check Empty _ = True
        check (Node x _ _) f = f x
