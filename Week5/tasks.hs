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
import Prelude hiding (words)
import Distribution.Simple.Command (OptDescr(BoolOpt))

-- Задача 1: Да се напише map (\x -> x * x) [1..5], изпозлвайки list comprehension
-- Задача 2: Да се напише filter even [1..10], изпозлвайки list comprehension
-- Задача 3: Да се напише функция, която връща всички букви от английската азбука индексирани [(1, 'а'), (2, 'b') ...]
-- Задача 4: Да се напише функция, която връща всички двойки (x, y) такива, че x и y са от 1 до 5 и x < y
-- Задача 5: Да се напише функция, която намира всички тройки (a, b, c) такива, че a*a + b*b = c*c и a,b,c са от 1 до 10

-- Задача 6: Изполвайки list comprehensio, да се напише функция, която връща само имената на хората, които са пълнолетни
names :: [String]
names = ["Alice", "Bob", "Pesho"]

ages :: [Integer]
ages = [10, 18, 25]

-- (*) Задача 7: Да се намери сумата на дължините на всички думи, започващи със същата буква. 
--               Резултатът трябва да е в следния формат: [('a', 8), ('b', 10), ('m', 18)]

words :: [String]
words = ["banana", "apple", "mango", "ant", "bear", "mosaic", "mission"]

-- Задача 8: Да се намери сумата на всички числа
nums :: [String]
nums = ["1", "10", "5", "2"]

-- Задача 9: Да се намери сумата на всички подсписъци и да се върне като списък от стрингове
manyNums :: [[String]]
manyNums = [["1", "10", "5", "2"], ["1", "2", "5", "2"], ["1", "10", "15", "2"]]

-- Задача 10: Parser
toParse :: [String]
toParse = ["user=bob", "age=20", "active=True"]
-- Oчакван резултат: ("bob", 20, True)

