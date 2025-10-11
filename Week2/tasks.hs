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
{-# HLINT ignore "Use splitAt" #-}


-- :t <function>
-- :info <type>

-- Припомняне на основни неща от първа седмица

length' :: [Int] -> Int
length' = undefined

factorial :: Int -> Int
factorial = undefined

and :: [Bool] -> Bool
and = undefined

reverseList :: [Int] -> [Int]
reverseList = undefined

-- Нови задачи

-- Как хаскел пази променливи
-- Разликата между data, type, (newtype - няма да го ползваме)

-- На който му е интересно: https://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell

data Person = Person String Int
    deriving (Show)

getName :: Person -> String
getName = undefined

birthday :: Person -> Person
birthday = undefined

type Point = (Int, Int)

moveRight :: Point -> Int -> Point
moveRight = undefined

data Shape = Circle Point Int | Rectangle Point Point
    deriving (Show)

areEqual :: Shape -> Shape -> Bool
areEqual = undefined

-- Задачи:

-- elem
isSubList :: [Int] -> [Int] -> Bool
isSubList = undefined

-- maximum
isMax :: Int -> [Int] -> Bool
isMax = undefined

-- Конструиране на списъци

makeList :: Int -> Int -> [Int]
makeList = undefined

-- helper: take n <list> -- взима първите n елемента от списъка
-- helper: drop n <list> -- пропуска първите n елемента от списъка

makeListInfinite :: Int -> [Int]
makeListInfinite = undefined

allEven :: [Int]
allEven = undefined

makeFactorials :: [Int] -> [Int]
makeFactorials = undefined

allFactorials :: [Int]
allFactorials = undefined

myMap :: (Int -> Int) -> [Int] -> [Int]
myMap = undefined

pack :: [Int] -> [[Int]]
pack = undefined

transpose' :: [[Int]] -> [[Int]]
transpose' = undefined

matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult = undefined

mergeSort :: [Int] -> [Int]
mergeSort = undefined











