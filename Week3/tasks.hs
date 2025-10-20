{-# LANGUAGE EmptyDataDeriving #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
--{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Eta reduce" #-}

-- Задачи с curry/uncurry

items :: [(Int, Int)]
items = [(5, 1), (2, 5), (3, 4), (6, 5) ]

additionalVals = [2, 5, 3, 2]

calculateItems val modifier = val * modifier

-- Задача 1: Приложете функцията calculateItems върху всеки от елементите на items
-- Задача 2: След като сте изпълнили първа задача добавете съответния елемент на additional_vals към резултата

items2 :: [Int]
items2 = [5, 2, 3, 6]

modifiers2 :: [Int]
modifiers2 = [1, 5, 4, 5]

calculateItems2 (val, modifier) = val * modifier

-- Задача 4: повторете задача 1 и 2 за items2, modifiers2 и calculateItems2

-- Задачи с Maybe:

-- Задача 1: да се дефинира монада Maybe
-- Задача 2: Напишете функция, която приема Maybe Int и връща Int (Nothing -> 0)
-- Задача 3: safe head/tail 
-- Задача 4: safe division
-- Задача 5: fmap


-- Задачи с Either

-- Задача 1: Да се дефинира монада Either
-- Задача 2: Да се дефинира функция isAnAdult, която
    -- приема възраст
    -- проверява дали е в интервала [0, 120]
    -- ако не е - връща грешка 
    -- ако е: Връща True ако възрастта е >=18 и False, иначе


-- Други задачи

-- Задача 1: Напишете функция, която връща третия елемент на списък 
-- Задача 2: Напишете функция, която приема списък от едноцифрени числа и ги конкатенира
-- Задача 3: Да се напише функцията filter
-- Задача 4: Да се приложи safeDiv върху елементите на 2 списъка. 
    -- Ако някъде се провали да се хвърли съобщение за грешка.
    -- В противен случай да се върне списък от резултатите
-- Задача 5: 
    -- а) Напишете тип за животно
    -- б) Напишете функция, която приема число и връща съответното животно
    -- в) Напишете обратната функция
    -- г) Напишете функция, която използва предните 2. Тя трябва да приема животно и да връща същото
-- Задача 6: Да се напише функция, която приема списък от стрингове и ги връща индексирани ["a", "b", "c"] -> [(1, "a"), (2, "b"), (3, "c")]
-- Задача 7: Напишете фунцкия, която приема стринг и премахва всички букви от началото му докато не стигне до число "sdf123" -> "123"