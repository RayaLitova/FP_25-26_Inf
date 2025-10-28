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

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use snd" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use zipWith" #-}

import Data.Char (isDigit)

-- От миналия път:

-- Задача 6: Да се напише функция, която приема списък от стрингове и ги връща индексирани ["a", "b", "c"] -> [(1, "a"), (2, "b"), (3, "c")]
zad6 :: [String] -> [(Int, String)]
zad6 [] = []
zad6 l = zip [1 ..] l

-- Задача 7: Напишете фунцкия, която приема стринг и премахва всички букви от началото му докато не стигне до число "sdf123" -> "123"

zad7 :: String -> String
zad7 s = dropWhile (not . isDigit) s

-- folds

-- Задача 1: Да се дефинира foldl

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b [] = b
foldl' f b (x : xs) = f (foldl' f b xs) x

-- Задача 2: Да се дефинира foldr

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ b [] = b
foldr' f b (x : xs) = f x (foldr' f b xs)


-- Задача 3: SumList [1,2,3] -> 6

sumList :: [Int] -> Int
sumList xs = foldl (+) 0 xs

-- Задача 3.1: ListProduct: [1,2,3] -> 6
productList :: (Foldable t, Num a) => t a -> a
productList xs = foldl (*) 1 xs

-- Задача 4: And [True, True, False] -> False
myAnd :: [Bool] -> Bool
myAnd xs = foldl (&&) True xs

-- Задача 5: reverse (ще използваме функцията flip)
rev :: [a] -> [a]
rev xs = foldl (flip (:)) [] xs

-- Ламбда функции

-- Задача 1: Да се напише ламбда функция, която добавя 1 към число
-- (\x -> x + 1)
-- Задача 2: Да се напише ламбда функция, която умножава две числа
-- (\x y -> x * y)

-- Задача 3: SumList с ламбда функция
sumList2 :: [Int] -> Int
sumList2 l = foldl (\x y -> x + y) 0 l

-- Задача 4: filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

-- Задача 5: map
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- Задача 5: assignGradesDS -> имате 2 контролни на всяко ви трябват поне 20% и общо ви трябват поне 40%
assignGradesDS :: [(String, Int, Int)] -> [(String, String)]
assignGradesDS xs = map (\(n, k1, k2) -> if k1 < 20 || k2 < 20 || k1 + k2 < 80 then (n ,"Fail") else (n, "Pass")) xs

-- Задача 6: Да се намери най-кратката дума ["Will", "I", "pass", "dstr", "?"] -> "I"
shortestWord :: [String] -> String
shortestWord [] = ""
shortestWord s@(x : _) = foldl (\acc a -> if length a >= length acc then acc else a) x s

-- Задачи от сборника
--28.2
type Triple = [(Int, Int, Int)]

l1 :: [(Int, Int, Int)]
l1 = [(1,1,1), (2,2,2), (3,4,5)]

sum' :: Triple -> [Int]
sum' l = map (\(x, y, z) -> x + y + z) l

sumElem :: Triple -> (Int, Int, Int)
sumElem l = foldl (\(ax, ay, az) (x, y, z) -> (ax + x, ay + y, az + z)) (0, 0, 0) l

countElem :: Triple -> Int
countElem l = foldl (\acc (x, y, z) -> if x + y > z then acc + 1 else acc) 0 l

hasEq :: Triple -> Bool
hasEq l = foldl (\acc (x, y, z) -> if x == y && y == z then True else acc) False l
-- hasEq l = foldl (\acc (x, y, z) -> acc || (x == y && y == z)) False l

--28.3
matchIdx :: [Int] -> [Int]
matchIdx l = map (\(_, y) -> y) (filter (\(x, y) -> x == y) (zip [1 ..] l))

--28.4
sumNeighbour :: [Int] -> [Int]
sumNeighbour l = zipWith (+) l (tail l)

-- Реално zipWith е същото като map(zip)
sumNeighbour2 :: [Int] -> [Int]
sumNeighbour2 l = map (\(x, y) -> x + y) (zip l (tail l))

-- 28.6
separate :: (a -> Bool) -> [a] -> ([a], [a])
separate f l = foldr (\x (xs, ys) -> if f x then (x : xs, ys) else (xs, x : ys)) ([], []) l
