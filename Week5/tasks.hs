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
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use map" #-}
import Prelude hiding (words)
import Text.Read (readMaybe) 

-- Задача 1: Да се напише map (\x -> x * x) [1..5], изпозлвайки list comprehension
squared :: [Int]
squared = [x * x | x <- [1 .. 5]]

-- Задача 2: Да се напише filter even [1..10], изпозлвайки list comprehension
filterEven :: [Int]
filterEven = [x | x <- [1 .. 10], even x]

-- Задача 3: Да се напише функция, която връща всички букви от английската азбука индексирани [(1, 'а'), (2, 'b') ...]
indexAlphabet :: [(Int, Char)]
indexAlphabet = zip [1 .. ] ['a' .. 'z']

-- Задача 4: Да се напише функция, която връща всички двойки (x, y) такива, че x и y са от 1 до 5 и x < y
allPairs :: [(Int, Int)]
allPairs = [(x, y) | x <- [1 .. 5], y <- [1 .. 5], x < y]

-- Задача 5: Да се напише функция, която намира всички тройки (a, b, c) такива, че a*a + b*b = c*c и a,b,c са от 1 до 10
squaredSum :: [(Int, Int, Int)]
squaredSum = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x * x + y * y == z * z]

-- Задача 6: Изполвайки list comprehension, да се напише функция, която връща само имената на хората, които са пълнолетни
names :: [String]
names = ["Alice", "Bob", "Pesho"]

ages :: [Integer]
ages = [10, 18, 25]

-- fst - Връща първия елемент на наредена !двойка!
-- snd - Връща втория елемент на наредена !двойка!

allAdults :: [String]
allAdults = [fst x | x <- zip names ages, snd x >= 18]

-- (*) Задача 7: Да се намери сумата на дължините на всички думи, започващи със същата буква. 
--               Резултатът трябва да е в следния формат: [('a', 8), ('b', 10), ('m', 18)]

words :: [String]
words = ["banana", "apple", "mango", "ant", "bear", "mosaic", "mission"]

sumSame :: [(Char, Int)]
-- sumSame = filter (\x -> snd x > 0)[(l, sum $ map length $ filter (\a -> head a == l) words) | l <- ['a' .. 'z']]
sumSame = [(l, sum $ map length $ filter (\a -> head a == l) words) | l <- ['a' .. 'z'], not . null $ filter (\a -> head a == l) words]

-- Задача 8: Да се намери сумата на всички числа
nums :: [String]
nums = ["1", "10", "5", "2"]

sumNums :: [String] -> Int
sumNums l = sum [read x | x <- l] 

-- Задача 9: Да се намери сумата на всички подсписъци и да се върне като списък от стрингове
manyNums :: [[String]]
manyNums = [["1", "10", "5", "2"], ["1", "2", "5", "2"], ["1", "10", "15", "2"]]

manySum :: [String]
manySum = [show $ sumNums x | x <- manyNums]

-- Задача 10: Parser
toParse :: [String]
toParse = ["user=bob", "age=20", "active=True"]
-- Oчакван резултат: ("bob", 20, True)

parser :: (String, Int, Bool)
parser = helper [drop 1 $ dropWhile (/='=') x | x <- toParse]
    where
        helper [x, y, z] = (x, read y, read z)
        helper _ = error "Oops"

-- Бонус: Да се пренапише задача 10, използвайки record
data User = User { name :: String, age :: Int, active :: Bool }
    deriving (Show)

parser2 :: User
parser2 = helper [drop 1 $ dropWhile (/='=') x | x <- toParse]
    where
        helper [x, y, z] = User {name = x, age = read y, active = read z}
        helper _ = error "Oops"

-- Бонус2: Използвайки readMaybe да се напише функция, която създава User от следните списъци: (name винаги е на първа позиция)
toParse2 :: [String]
toParse2 = ["bob", "20", "True"]
toParse3 :: [String]
toParse3 = ["bob", "True", "20"]

maybeOr :: [Maybe a] -> a
maybeOr [] = error "oops"
maybeOr (Just a : _) = a
maybeOr (Nothing : xs) = maybeOr xs

parser3 :: [String] -> User
parser3 (name : xs) = User {name = name, age = age, active = active}
    where 
        age :: Int
        age = maybeOr $ map readMaybe xs
        active :: Bool
        active = maybeOr $ map readMaybe xs

parser3 _ = error "Oops"
--parser3 [name, d1, d2] = if (readMaybe d1 :: Maybe Int) == Nothing then
--                            User {name = name, age = read d2, active = read d1}
--                         else 
--                            User {name = name, age = read d1, active = read d2}

-- Бонус 3: Да се направи функция, която приема списък от списъци, в които има toParseUser и ги превръща в списък от Users
toParse10 :: [String]
toParse10 = ["user=bob", "active=True", "age=20"]

toParse11 :: [String]
toParse11 = ["user=bob", "age=20", "active=True", "test=uhu"]

toParse12 :: [String]
toParse12 = ["user=bob", "test=uhu", "age=20", "active=True"]

specialWords :: [String]
specialWords = ["user", "age", "active"]

parser4 :: [[String]] -> [User]
parser4 [] = []
parser4 (l : ls) = parser3 [drop 1 $ dropWhile (/='=') x | x <- l, takeWhile (/='=') x `elem` specialWords] : parser4 ls

