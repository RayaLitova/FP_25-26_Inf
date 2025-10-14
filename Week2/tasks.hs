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
length' [] = 0
length' (_ : xs) = 1 + length' xs 

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && (myAnd xs)

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x] 

-- Нови задачи

-- Как хаскел пази променливи
-- Разликата между data, type, (newtype - няма да го ползваме)

-- На който му е интересно: https://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell

data Person = Person String Int
    deriving (Show)

getName :: Person -> String
getName (Person name _) = name

birthday :: Person -> Person
birthday (Person name age) = Person name (age + 1) 

type Point = (Int, Int)

moveRight :: Point -> Int -> Point
moveRight (x, y) a = (x+a, y) 

data Shape = Circle Point Int | Rectangle Point Point
    deriving (Show, Eq)

areEqual :: Shape -> Shape -> Bool
areEqual s1 s2 = s1 == s2

-- Задачи:

-- elem
isSubList :: [Int] -> [Int] -> Bool
isSubList [] _ = True
isSubList (x : xs) ys = if notElem x ys then False else isSubList xs ys 

-- maximum
isMax :: Int -> [Int] -> Bool
isMax x ys = x == maximum ys 

-- Конструиране на списъци

makeList :: Int -> Int -> [Int]
makeList _ 0 = []
makeList x n = x : makeList x (n-1) 

makeList' :: Int -> Int -> [Int]
makeList' _ 0 = []
makeList' x n = take n [x, x ..]

-- helper: take n <list> -- взима първите n елемента от списъка
-- helper: drop n <list> -- пропуска първите n елемента от списъка

makeListInfinite :: Int -> [Int]
makeListInfinite x = [x, x ..]

allEven :: [Int]
allEven = [2,4 ..]

makeFactorials :: [Int] -> [Int]
makeFactorials xs = myMap factorial xs  

allFactorials :: [Int]
allFactorials = myMap factorial [1..]

add1 :: Int -> Int
add1 x = x + 1

myMap :: (Int -> Int) -> [Int] -> [Int]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

-- takeWhile, dropWhile
pack :: [Int] -> [[Int]]
pack [] = []
pack (x : xs) = takeWhile (== x) (x : xs) : pack (dropWhile (==x) xs)

-- head - first element of a list
-- tail - the rest
transpose' :: [[Int]] -> [[Int]]
transpose' [] = []
transpose' ([] : _) = []
transpose' m = map head m : transpose' (map tail m)

getElem :: [Int] -> [Int] -> Int
getElem [] _ = 0
getElem _ [] = 0
getElem (x : xs) (y : ys) = x * y + getElem xs ys

getRow :: [Int] -> [[Int]] -> [Int]
getRow [] _ = []
getRow _ [] = []
getRow xs (ys : yss) = getElem xs ys : getRow xs yss

matrixMult :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult [] _ = []
matrixMult _ [] = []
matrixMult (xs : xss) m2 = getRow xs (transpose' m2) : matrixMult xss m2

matrixMult' :: [[Int]] -> [[Int]] -> [[Int]]
matrixMult' [] _ = []
matrixMult' _ [] = []
matrixMult' (xs : xss) m2 = map sum (map (zipWith (*) xs) m2) : matrixMult' xss m2

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort $ take l xs) (mergeSort $ drop l xs)
    where l = length xs `div` 2 

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
 | x < y = x : merge xs (y : ys)
 | otherwise = y : merge (x : xs) ys 

union :: [Int] -> [Int] -> [Int]
union [] ys = ys
union (x : xs) ys = if elem x ys then union xs ys else x : union xs ys

intersection :: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection (x : xs) ys = if notElem x ys then intersection xs ys else x : intersection xs ys

flattenL :: [[Int]] -> [Int]
flattenL [] = []
flattenL (xs : xss) = xs ++ flattenL xss





