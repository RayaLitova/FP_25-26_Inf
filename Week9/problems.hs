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
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use isJust" #-}
import Data.Char (isDigit)

-- Да се имплементират Stack и Queue

type Stack a = [a]

stackPush :: Stack a -> a -> Stack a
stackPush s a = a : s

stackPop :: Stack a -> Stack a
stackPop [] = []
stackPop (_ : xs) = xs
 
type Queue a = [a]

enqueue :: Queue a -> a -> Queue a 
enqueue q a = q ++ [a]

dequeue :: Queue a -> Queue a
dequeue [] = []
dequeue (_ : xs) = xs

-- Какво е parser?
    -- parser is a function for parsing - chat gpt 2025
-- Какво е parser combinator?

-- Да се създаде клас Parser
class Parser p where
    run :: String -> p -> Maybe Int

-- Да се създаде тип ArithmeticParser
    -- Целта на ArithmeticParser е по подаден списък от оператори да пресмята изрази
    -- (оператор наричаме наредена тройка от символ, приоритет и операция)

type Operator = (Char, Int, Int -> Int -> Int)

getSymbol :: Operator -> Char
getSymbol (x, _, _) = x

getPriority :: Operator -> Int
getPriority (_, x, _) = x

getFunc :: Operator -> Int -> Int -> Int
getFunc (_, _, x) = x

getOperator :: [Operator] -> Char -> Operator
getOperator _ '(' = ('(', 0, undefined)
getOperator _ ')' = (')', 0, undefined)
getOperator (x : xs) c = if getSymbol x == c then x else getOperator xs c
getOperator [] _ = error "Unknown symbol"

data ArithmeticParser = ArithmeticParser [Operator]

instance Parser ArithmeticParser where
    run :: String -> ArithmeticParser -> Maybe Int
    run s a = 
        case shuntingYard a s of
            Nothing -> Nothing
            Just r -> Just $ evalRevPolish a r
        
operators :: [Operator]
operators = [('+', 1, (+)), ('-', 1, (-)), ('*', 2, (*)), ('/', 2, div)]

parser1 :: ArithmeticParser 
parser1 = ArithmeticParser operators

shuntingYard :: ArithmeticParser -> String -> Maybe String
shuntingYard _ "" = Just ""
shuntingYard (ArithmeticParser ops) l = helper l [] []
    where 
        helper :: String -> Queue Char -> Stack Char -> Maybe String
        helper "" q s = Just $ q ++ s
        helper (x : xs) q s 
            | x `elem` map getSymbol ops = 
                let
                    getOpPriority = getPriority . getOperator ops
                    p = getOpPriority x
                in 
                    helper xs (q ++ takeWhile (\y -> getOpPriority y >= p) s) (stackPush (dropWhile (\y -> getOpPriority y >= p) s) x)
            | x == '(' = helper xs q (stackPush s x)
            | x == ')' = helper xs (q ++ takeWhile (/='(') s) (drop 1 $ dropWhile (/='(') s)
            | isDigit x = helper xs (enqueue q x) s 
            | otherwise = Nothing

-- Какво представлява shunting yard алгоритъма?
-- Какво представлява reversed polish notation?

getFirstTwo :: Stack Int -> ((Int, Int), Stack Int)
getFirstTwo [] = error "a"
getFirstTwo [_] = error "oops"
getFirstTwo (x : y : xs) = ((y, x), xs)

evalRevPolish :: ArithmeticParser -> String -> Int 
evalRevPolish (ArithmeticParser ops) l = helper l []
    where 
        helper :: String -> Stack Int -> Int
        helper [] [] = error "empty stack"
        helper [] [x] = x
        helper xs s
            | isDigit $ head xs = helper (dropWhile isDigit xs) (reverse (map (\y -> read [y]) (takeWhile isDigit xs)) ++ s)
            | otherwise = helper (tail xs) (uncurry f els : ns)
                where 
                    f = getFunc $ getOperator ops $ head xs
                    (els, ns) = getFirstTwo s

-- Да се инстанцира Parser за ArithmeticParser (за улеснение ще работим само с едноцифрени числа)
-- Да се направят две инстанции на ArithmeticParser
    -- Първата да работи със стандартните оператори (+, -, /, *)
    -- Втората да работи с 2 случайни символа (ще си ги изберем на упражнението)

parser2 :: ArithmeticParser 
parser2 = ArithmeticParser [('!', 1, mod), ('&', 2, max)]

-- Да се създаде тип ParserCombinatorOr, който наследява Parser
    -- Той трябва да съдържа списък от Parser-и и да връща резултата на първия, който успее да изчисли израза

data ParserCombinatorOr = ParserCombinatorOr [ArithmeticParser]

parserComb :: ParserCombinatorOr 
parserComb = ParserCombinatorOr [parser1, parser2]

instance Parser ParserCombinatorOr where
    run :: String -> ParserCombinatorOr -> Maybe Int
    run _ (ParserCombinatorOr []) = Nothing 
    run s (ParserCombinatorOr (x : xs))
        | run s x /= Nothing = run s x
        | otherwise = run s (ParserCombinatorOr xs)

-- Да се напише функция, която по подаден резултат генерира всички изрази, които водят до него
generateAll :: ArithmeticParser -> Int -> [String]
generateAll (ArithmeticParser ops) m = concatMap (helper m) [1..]
    where 
        helper :: Int -> Int -> [String]
        helper n 1 = [show n]
        helper n i = [ show x ++ [getSymbol z] ++ "(" ++ t ++ ")" | x <- [1..9], y <- [1..9], z <- ops, getFunc z x y == n, t <- helper y (i - 1)]

-- Да се напише функция, която чете израз от стандартния вход и го оценява
readFromConsole :: ArithmeticParser -> IO ()
readFromConsole a = do
    x <- getLine
    print (run x a) 

-- Да се напише функция, която чете стойности от CSV файл и спрямо колоните и редовете преценя, коя операция да извърши
readFromCSV :: ArithmeticParser -> [Operator] -> IO ()
readFromCSV a ops = do
    content <- readFile "./Week9/exampleCSV.csv"
    print $ run (init $ concat $ zipWith (\x y -> [x, y]) (filter isDigit content) (cycle (map getSymbol ops))) a
