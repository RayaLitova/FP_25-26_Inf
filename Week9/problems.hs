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
import Data.Char (isDigit)

-- Да се имплементират Stack и Queue

-- Какво е parser?
-- Какво е parser combinator?

-- Да се създаде клас Parser
-- Да се създаде тип ArithmeticParser
    -- Целта на ArithmeticParser е по подаден списък от оператори да пресмята изрази
    -- (оператор наричаме наредена тройка от символ, приоритет и операция)

-- Какво представлява shunting yard алгоритъма?
-- Какво представлява reversed polish notation?

-- Да се инстанцира Parser за ArithmeticParser (за улеснение ще работим само с едноцифрени числа)
-- Да се направят две инстанции на ArithmeticParser
    -- Първата да работи със стандартните оператори (+, -, /, *)
    -- Втората да работи с 2 случайни символа (ще си ги изберем на упражнението)

-- Да се създаде тип ParserCombinatorOr, който наследява Parser
    -- Той трябва да съдържа списък от Parser-и и да връща резултата на първия, който успее да изчисли израза

-- Да се напише функция, която по подаден резултат генерира всички изрази, които водят до него

-- Да се напише функция, която чете израз от стандартния вход и го оценява
-- Да се напише функция, която чете стойности от CSV файл и спрямо колоните и редовете преценя, коя операция да извърши