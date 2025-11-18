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
-- Задача 2: Да се дефинира тип за дърво

-- За следващите задачи дърво ще означава бинарно дърво
--tree :: BTree
--tree = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 22 (Node 33 Empty Empty) (Node 44 Empty Empty))

-- Задача 3: Да се напише функция, която връща броя на елементите на дърво, 
    -- които удовлетворяват даден предикат
-- Задача 4: Да се напише функция, която приема бинарно дърво и връща сумата на всичките му листа
-- Задача 5: Да се напише функция, която намира най-големия по стойност елемент на дърво. 
    -- Приемете, че всички стойности са положителни
-- Задача 6: Да се напише функция, която приема списък и връща дърво
