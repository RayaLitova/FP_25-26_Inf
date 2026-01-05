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

-- Следващия път ще имаме 4 часа
-- Въпроси?

-- Задача 1: Да се създаде клас Recipe, който съдържа функциите evaluate :: Recipe -> Int и modify :: Recipe -> String -> Effect -> Recipe 
-- Задача 2: Да се дефинира тип Potion, който представлява дърво (ще дам повече разяснения по време на упражнението)
-- Задача 3: Да се инстанцира Recipe за Potion.
-- evaluate оценява финалния ефект по следните правила:
    -- Heal - 1 
    -- Poison - -1
    -- Neutral - 0 
    -- Ако една съставка се съдържа повече от 1 път то тогава тя се оценява до: ефекта си * броя на срещанията
-- modify подменя ефекта на всяка съставка с подаденото име с подадения ефект

-- Задача 4: Да се дефинира тип PlayerHealth
-- Задача 5: Да се напише функция takePotion, която приема PlayerHealth и Potion и принтира подходящо съобщение за ефекта на Potion-a

-- Задача 6: Да се дефинира тип ComplexPotion, който представлява матрица от Potion
-- Задача 7: Да се напише функция evalComplexPotion, която приема ComplexPotion и връща Int. Оценяването става по следните правила:
    -- Всеки ред, колона и диагонал се смятат като 1 Potion.
    -- Резултатът е сумата им
-- Задача 8: Да се напише функция modifyComplexPotion (работи по същия начин като modify)