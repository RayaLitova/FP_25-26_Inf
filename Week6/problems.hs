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

-- Enum - Репрезентация от и към Int
-- Bounded - Най-малък и най-голям елемент
data WeekDays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Read, Enum, Bounded)


-- use toEnum and fromEnum
nextDay :: WeekDays -> WeekDays
nextDay d = toEnum ((fromEnum d + 1) `mod` 7)

-- minBound
firstDay :: WeekDays
firstDay = minBound :: WeekDays

-- maxBound
lastDay :: WeekDays
lastDay = maxBound

-- IO
-- Каква е разликата между pure и inpure функция?
-- Сигнатура IO срещу обикновена сигнатура
-- IO ()

-- Каква е сигнатурата на функция, която принтира на екрана?
-- print :: IO ()
-- Каква е сигнатурата на функция, която чете String от конзолата?
-- read :: IO String

-- Задача 1: Да се напише функция, чете изречение от конзолата и му слага точка накрая, ако няма такава
readSentense :: IO String
readSentense = do
    s <- getLine
    if last s == '.' then
        return s
    else
        return (s ++ ['.'])

-- Задача 2: Да се напише функция, която чете от конзолата ден от седмицата и принтира следващия ден
readWeekDay :: IO ()
readWeekDay = do
    s <- getLine
    print $ nextDay $ read s

-- Задача 3: Да се напише функция, която чете от конзолата две числа и принтира тяхната сума
readAndSum :: IO ()
readAndSum = do
    putStrLn "Enter number 1"
    x <- getLine
    putStrLn "Enter number 2"
    y <- getLine
    print (read x + read y)

-- pure и return
-- Задача 4: Да се напише функция, която приема булев аргумент (needsInput). 
    -- Ако той е истина, чете String от конзолата и го връща
    -- В противен случай връща "Default name"

readName :: Bool -> IO String
readName needsInput =
    if needsInput then
        getLine
    else
        return "Default name"

-- Задача 5: Да се напише функция, която чете цяло число n от конзолата, след това чете n числа и връща тяхната сума
readNAndSum :: IO Int
readNAndSum = do
    n <- getLine
    helper $ read n
    where
        helper :: Int -> IO Int
        helper n = do
            x <- getLine
            y <- helper (n-1)
            return $ read x + y



-- Задача 6: Да се напише функция, която чете цяло число n от конзолата, след това чете n изречения и ги принтира в обратен ред
readNSentenses :: IO ()
readNSentenses = do
    n <- getLine
    s <- helper (read n)
    let r = reverse s
    helper2 r
    where
        helper :: Int -> IO [String]
        helper 1 = do
            x <- getLine
            return [x]
        helper n = do
            x <- getLine
            y <- helper (n-1)
            return $ x : y

        helper2 :: [String] -> IO ()
        helper2 [] = error "oops"
        helper2 [x] = print x
        helper2 (x:xs) = do
            print x
            helper2 xs

createFn :: Eq a => [(a, b)] -> a -> b
createFn xs x = snd $ head $ filter (\(a, _) -> a == x) xs