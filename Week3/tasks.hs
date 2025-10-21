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
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use !!" #-}
{-# HLINT ignore "Use foldr" #-}
import Data.Char (isDigit)

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Eta reduce" #-}

-- Задачи от група 1

isInfixOf :: [Int] -> [Int] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf a b@(_ : ys)
 | isPrefixOf a b = True
 | otherwise = isInfixOf a ys

isPrefixOf :: [Int] -> [Int] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = if x /= y then False else isPrefixOf xs ys

-- приемаме, че всички елементи на втория списък са уникални
isInfixOf2 :: [Int] -> [Int] -> Bool
isInfixOf2 [] _ = True
isInfixOf2 _ [] = False
isInfixOf2 p@(x : _) ys = take (length p) r == p
    where r = dropWhile (/= x) ys

-- Задачи с curry/uncurry

-- Задача 1: Приложете функцията calculateItems върху всеки от елементите на items
-- Задача 2: След като сте изпълнили първа задача добавете съответния елемент на additional_vals към резултата

items :: [(Int, Int)]
items = [(5, 1), (2, 5), (3, 4), (6, 5) ]

additionalVals :: [Int]
additionalVals = [2, 5, 3, 2]

calculateItems :: Int -> Int -> Int
calculateItems val modifier = val * modifier

zad1 :: [Int]
zad1 = zipWith (+) (map (uncurry calculateItems) items) additionalVals

-- Задача 4: повторете задача 1 и 2 за items2, modifiers2

items2 :: [Int]
items2 = [5, 2, 3, 6]

modifiers2 :: [Int]
modifiers2 = [1, 5, 4, 5]

calculateItems2 :: (Int, Int) -> Int
calculateItems2 (val, modifier) = val * modifier

zad2 :: [Int]
zad2 = zipWith (+) (map calculateItems2 (zip items2 modifiers2)) additionalVals

-- Вариант 2 (с използване на curry)
zad2' :: [Int]
zad2' = zipWith (+) (zipWith (curry calculateItems2) items2 modifiers2) additionalVals

-- Задачи с Maybe:

-- Задача 1: да се дефинира монада Maybe
data MyMaybe = MyNothing | MyJust Int

-- Задача 2: Напишете функция, която приема Maybe Int и връща Int (Nothing -> 0)
toInt :: Maybe Int -> Int
toInt Nothing = 0
toInt (Just x) = x

-- Задача 3: safe head 

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- Задача 4: safe division
safeDivision :: Int -> Int -> Maybe Int
safeDivision _ 0 = Nothing
safeDivision a b = Just (a `div` b)

-- Задача 5: fmap
myFmap :: (a -> b) -> Maybe a -> Maybe b
myFmap _ Nothing = Nothing
myFmap f (Just a) = Just (f a)

-- Задачи с Either

-- Задача 1: Да се дефинира монада Either
data MyEither a b = MyLeft a | MyRight b

-- Задача 2: Да се дефинира функция isAnAdult, която
    -- приема възраст
    -- проверява дали е в интервала [0, 120]
    -- ако не е - връща грешка (string)
    -- ако е: Връща True ако възрастта е >=18 и False, иначе

isAnAdult :: Int -> Either Bool String
isAnAdult x
 | x < 0 || x > 120 = Right "error"
 | otherwise = Left (x >= 18)

-- Други задачи

-- Задача 1: Напишете функция, която връща третия елемент на списък 

-- Вградена фунцкия: !!
idx :: [a] -> Maybe a
idx xs
 | length xs < 3 = Nothing
 | otherwise = Just $ head (drop 3 xs)


-- Задача 2: Напишете функция, която приема списък от списъци и ги конкатенира
-- Вградена функция: concat
cct :: [[a]] -> [a]
cct [] = []
cct (xs : xss) = xs ++ cct xss

-- Задача 3: Да се напише функцията filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x : xs) = if f x then x : myFilter f xs else myFilter f xs

-- Задача 4: Да се приложи safeDiv върху елементите на 2 списъка. 
    -- Ако някъде се провали да се хвърли съобщение за грешка.
    -- В противен случай да се върне списък от резултатите

applySafeDiv :: [Int] -> [Int] -> [Int]
applySafeDiv [] _ = []
applySafeDiv _ [] = []
applySafeDiv (x : xs) (y : ys) =
    case safeDivision x y of
        Nothing -> error "greshka"
        Just a -> a : applySafeDiv xs ys

-- Задача 5: 
    -- а) Напишете тип за животно
    -- б) Напишете функция, която приема число и връща съответното животно
    -- в) Напишете обратната функция
    -- г) Напишете функция, която използва предните 2. Тя трябва да приема животно и да връща същото

data Animal = Cat | Dog | Mice
    deriving Show

numToAnimal :: Int -> Animal
numToAnimal 1 = Cat
numToAnimal 2 = Dog
numToAnimal 3 = Mice
numToAnimal _ = error "cat"

animalToNum :: Animal -> Int
animalToNum Cat = 1
animalToNum Dog = 2
animalToNum Mice = 3

final :: Animal -> Animal
final a = (numToAnimal . animalToNum) a

-- За домашно: 

-- Задача 6: Да се напише функция, която приема списък от стрингове и ги връща индексирани ["a", "b", "c"] -> [(1, "a"), (2, "b"), (3, "c")]
-- Задача 7: Напишете фунцкия, която приема стринг и премахва всички букви от началото му докато не стигне до число "sdf123" -> "123"