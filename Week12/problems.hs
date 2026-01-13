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
{-# HLINT ignore "Replace case with fromMaybe" #-}

-- Задача 1: Да се дефинира тип за судоку дъска
-- Задача 2: Да се напише функция, която при подадени ред и колона връща всички възможни числа, 
--           които могат да бъдат на тази позиция
-- Задача 3: Да се дефинира тип за дърво от судоку дъски 
-- Задача 4: Да се напише функция, която генерира дърво от судоку дъска. 
-- Коренът на дървото трябва да бъде оригиналната дъска
-- Започваме от клетка 0,0 и обхождаме по редове
-- Нека текущата позиция е (x, y). 
-- Нивото на дървото, на което се намираме в момента, трябва да съдържа всички варианти на дъската,
-- в които клетка (x, y) е запълнена с валидно число

-- Задача 5: Да се дефинира функция, която обхожда дървото и връща първата срещната запълнена дъска,
-- ако има такава
-- Задача 6: Да се дефинира функция, която приема дъска за судоку и връща нейното решение

