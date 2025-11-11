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
import GHC.Base (VecElem(Int16ElemRep))
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use map" #-}

-- Записахте ли се за контролното?

-- Enum and Bounded
data WeekDays = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Read, Enum, Bounded)

-- use toEnum and fromEnum
nextDay :: WeekDays -> WeekDays
nextDay = undefined

-- minBound
firstDay :: WeekDays
firstDay = undefined

-- maxBound
lastDay :: WeekDays
lastDay = undefined

-- IO
-- Каква е разликата между pure и inpure функция?
-- Сигнатура IO срещу обикновена сигнатура
-- IO ()

-- Каква е сигнатурата на функция, която принтира на екрана?
-- Каква е сигнатурата на функция, която чете String от конзолата?

-- getName :: 
-- printName ::

-- Задача 1: Да се напише функция, чете изречение от конзолата и му слага точка накрая, ако няма такава
-- Задача 2: Да се напише функция, която чете от конзолата ден от седмицата и принтира следващия ден
-- Задача 3: Да се напише функция, която чете от конзолата две числа и принтира тяхната сума

-- pure и return
-- Задача 4: Да се напише функция, която приема булев аргумент (needsInput). 
    -- Ако той е истина, чете String от конзолата и го връща
    -- В противен случай връща "Default name"

-- Задача 5: Да се напише функция, която чете цяло число n от конзолата, след това чете n числа и връща тяхната сума
-- Задача 6: Да се напише функция, която чете цяло число n от конзолата, след това чете n изречения и ги принтира в обратен ред