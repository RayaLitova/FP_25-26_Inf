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


-- От миналия път:

-- Задача 6: Да се напише функция, която приема списък от стрингове и ги връща индексирани ["a", "b", "c"] -> [(1, "a"), (2, "b"), (3, "c")]
-- Задача 7: Напишете фунцкия, която приема стринг и премахва всички букви от началото му докато не стигне до число "sdf123" -> "123"

-- folds

-- Задача 1: Да се дефинира foldl
-- Задача 2: Да се дефинира foldr

-- Задача 3: SumList [1,2,3] -> 6
-- Задача 4: And [True, True, False] -> False
-- Задача 5: reverse (ще използваме функцията flip)

-- Ламбда функции
-- Задача 1: Да се напише ламбда функция, която добавя 1 към число
-- Задача 2: Да се напише ламбда функция, която умножава две числа
-- Задача 3: SumList с ламбда функция
-- Задача 4: filter
-- Задача 5: map

-- Задача 5: assignGradesDS -> имате 2 контролни на всяко ви трябват поне 20% и общо ви трябват поне 40%
-- Задача 6: Да се намери най-кратката дума ["Will", "I", "pass", "dstr", "?"] -> "I"
