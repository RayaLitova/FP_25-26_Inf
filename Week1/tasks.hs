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

add1 :: Int -> Int
add1 x = x + 1

add1' :: Int -> Int
add1' x = succ x

mySum :: Int -> Int -> Int
mySum x y = x + y

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

is5 :: Int -> Bool
is5 x = if x == 5 then True
        else False

-- Game

-- Actions
-- 1 -> take damage
-- 2 -> heal
-- 3 -> take damage 2 times
-- 4 -> take damage equal to the fifth number of fibonacci
-- 5 -> if your health is below 5 : heal for 5, else : take 5 damage
-- 6 -> if your health is a prime number take 20 damage, 
        -- if your health is divisble by 2 : heal for 10, 
        -- otherwise take damage equal to the smallest divisor

takeDamageNTimes :: Int -> Int -> Int
takeDamageNTimes x 0 = x
takeDamageNTimes x n = takeDamageNTimes (x-5) (n-1)

isPrime :: Int -> Bool
isPrime 1 = True
isPrime n = not (hasDivisors n (div n 2))
    where
        hasDivisors :: Int -> Int -> Bool
        hasDivisors _ 1 = False
        hasDivisors x y = if rem x y == 0 then True
        else hasDivisors x (y - 1)

getSmallestDivisor :: Int -> Int
getSmallestDivisor a = helper a 2
    where
        helper :: Int -> Int -> Int
        helper x y = if rem x y == 0 then y
        else helper x (y+1)

actions :: Int -> Int -> Int
actions _ 0 = 0
actions x y
    | x == 1 = y - 5
    | x == 2 = y + 5
    | x == 3 = takeDamageNTimes y 2
    | x == 4 = y - fib 5
    | x == 5 =  if y < 5 then y + 5
                else y - 5
    | otherwise = if isPrime y then y - 20
                  else if even y then y + 10
                  else y - getSmallestDivisor y

-- data examples
-- cannot be shown in the terminal:
data Animal = Dog | Cat | Parrot
-- can be shown in the terminal:
data Nat = Zero | Succ Nat
    deriving (Show)

-- example usage of Nat
numToNat :: Int -> Nat
numToNat 0 = Zero
numToNat n = Succ (numToNat (n-1))

---- 22.3. Да се дефинира функция, която има стойност истина, ако посоченото условие е вярно и стойност - лъжа, в противен случай:

-- цялото число p се дели на 4 или на 7
a22 :: Int -> Bool
a22 x = rem x 4 == 0 || rem x 7 == 0

-- уравнението ax2 + bx + c = 0(a ̸= 0) няма реални корени
b22 :: Int -> Int -> Int -> Bool
b22 a b c = b ^ 2 - 4 * a * c < 0

-- точка с координати (a, b) лежи извън кръга с център (c, d) и радиус f;
type Point = (Int, Int)

c22 :: Point -> Point -> Int -> Bool
c22 (x, y) (h, k) r = (x - h) ^ 2 + (y - k) ^ 2 < r ^ 2

-- x е равно на максималния елемент на списъка l
j22 :: Int -> [Int] -> Bool
j22 x l = x == maxElem l
    where
        maxElem :: [Int] -> Int
        maxElem [] = 0
        maxElem [a] = a
        maxElem (a : as) = max a (maxElem as)

-- нито едно от числата в списъка не е положително
i22 :: [Int] -> Bool
i22 [] = True
i22 (x : xs) =  if x > 0 then False
                else i22 xs

-- div - целочислено деление!
-- цифрата 7 влиза в записа на числото х
k22 :: Int -> Bool
k22 0 = False
k22 x = if rem x 10 == 7 then True
        else k22 (div x 10)

-- цифрите на числото х са различни
findElem :: Int -> [Int] -> Bool
findElem _ [] = False
findElem n (x : xs) = if x == n then True
                      else findElem n xs

l22 :: Int -> Bool
l22 x = helper x []
    where
        helper :: Int -> [Int] -> Bool
        helper 0 _ = True
        helper n xs = if findElem m xs then False
                else helper (div n 10) (m : xs)
                where m = rem n 10

-- цифрите на числото x образуват строго растяща или строго намаляваща редица
n22_1 :: Int -> Bool
n22_1 0 = True
n22_1 x = if div (rem x 100) 10 < rem x 10 then n22_1 (div x 10)
          else False

n22_2 :: Int -> Bool
n22_2 x
 | x < 10 = True
 | div (rem x 100) 10 > rem x 10 = n22_2 (div x 10)
 | otherwise = False

n22 :: Int -> Bool
n22 x = n22_1 x || n22_2 x

-- ++ - конкатенация на листове
-- десетичните записи числата x и y са симетрични;
breakNum :: Int -> [Int]
breakNum 0 = []
breakNum x = breakNum (div x 10) ++ [rem x 10]

assembleNum :: [Int] -> Int
assembleNum [] = 0
assembleNum [x] = x
assembleNum (x : xs) = (assembleNum xs * 10) + x

reverseNum :: Int -> Int
reverseNum x = assembleNum (breakNum x)

o22 :: Int -> Int -> Bool
o22 x y = reverseNum x == y








