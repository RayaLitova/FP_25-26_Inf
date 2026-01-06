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

data Effect = Heal | Poison | Neutral
    deriving (Show, Eq)

-- Задача 1: Да се създаде клас Recipe, който съдържа функциите evaluate :: Recipe -> Int и modify :: Recipe -> String -> Effect -> Recipe 
class Recipe r where
    evaluate :: r -> Int
    modify :: String -> Effect -> r -> r

-- Задача 2: Да се дефинира тип Potion, който представлява дърво (ще дам повече разяснения по време на упражнението)
data Potion = Ingredient String Effect | Combine Potion Potion
    deriving (Show)

examplePotion :: Potion
examplePotion = Combine (Ingredient "a" Heal) $ Combine (Ingredient "a" Heal) (Combine (Ingredient "b" Poison) (Ingredient "b" Poison))

editList :: [(String, Int)] -> String -> [(String, Int)]
editList [] s = [(s, 1)]
editList (x : xs) s
    | s == fst x = (s, snd x + 1) : xs
    | otherwise = x : editList xs s

-- null - prazen spisuk
-- any - or na celiq spisuk
-- all - and na celiq spisuk

find :: [(String, Int)] -> String -> Maybe (String, Int)
find l s = if null filtered then Nothing else Just (head filtered)
    where filtered = filter (\(i, _) -> i == s) l

effectToInt :: Effect -> Int
effectToInt Heal = 1
effectToInt Poison = -1
effectToInt Neutral = 0

-- Задача 3: Да се инстанцира Recipe за Potion.
instance Recipe Potion where
    evaluate :: Potion -> Int
    evaluate potion = snd $ helper potion []
        where
            helper :: Potion -> [(String, Int)] -> ([(String, Int)], Int)
            helper (Combine p1 p2) l = (finalList, i + j)
                where
                    (newList, i) = helper p1 l
                    (finalList, j) = helper p2 newList
            helper (Ingredient name effect) l =
                case find l name of
                    Just (n, c) -> (editList l n, effectToInt effect * (c + 1))
                    Nothing -> (editList l name, effectToInt effect)

    modify :: String -> Effect -> Potion -> Potion
    modify s e (Combine p1 p2) = Combine (modify s e p1) (modify s e p2)
    modify s e i@(Ingredient n _) = if n == s then Ingredient n e else i

-- evaluate оценява финалния ефект по следните правила:
    -- Heal - 1 
    -- Poison - -1
    -- Neutral - 0 
    -- Ако една съставка се съдържа повече от 1 път то тогава тя се оценява до: ефекта си * броя на срещанията
-- modify подменя ефекта на всяка съставка с подаденото име с подадения ефект

-- Задача 4: Да се дефинира тип PlayerHealth
type PlayerHealth = Int

-- Задача 5: Да се напише функция takePotion, която приема Potion, чете PlayerHeath от терминала и
    -- принтира подходящо съобщение за ефекта на Potion-a

takePotion :: Potion -> IO ()
takePotion p = do
    let evaluatedPotion = evaluate p
    healthStr <- getLine
    let playerHealth = read healthStr :: Int
    if evaluatedPotion > 0 then
        print $ "Healing. New player health: " ++ show (playerHealth + evaluatedPotion)
    else if evaluatedPotion == 0 then
        print $ "Neutral. New player health: " ++ show (playerHealth + evaluatedPotion)
    else
        print $ "Poison. New player health: " ++ show (playerHealth + evaluatedPotion)


-- Задача 6: Да се дефинира тип ComplexPotion, който представлява матрица от Potion
type ComplexPotion = [[Potion]]

exampleComplexPotion :: [[Potion]]
exampleComplexPotion =
    [
        [Ingredient "a" Heal, Ingredient "b" Poison],
        [Ingredient "b" Neutral, Ingredient "a" Heal]
    ]

exampleComplexPotion2 :: [[Potion]]
exampleComplexPotion2 =
    [
        [Combine (Ingredient "a" Heal) (Combine (Ingredient "a" Poison) (Ingredient "b" Heal)), Ingredient "b" Neutral],
        [Ingredient "a" Heal, Ingredient "b" Neutral]
    ]


-- Задача 7: Да се напише функция evalComplexPotion, която приема ComplexPotion и връща Int. Оценяването става по следните правила:
    -- Всеки ред, колона и диагонал се смятат като 1 Potion.
    -- Резултатът е сумата им
evalComplexPotion :: ComplexPotion -> Int
evalComplexPotion c = sum (map combine c) + sum (map combine $ transpose c) +
                        combine [c !! i !! i | i <- [0 .. length (head c) - 1]] + 
                        combine [c !! i !! (length (head c) - i - 1)| i <- [0 .. length (head c) - 1]]
    where
        combine :: [Potion] -> Int
        combine l = evaluate $ foldl1 Combine l

        transpose :: [[a]] -> [[a]]
        transpose [] = []
        transpose ([] : _) = []
        transpose m = map head m : transpose (map tail m)

-- Задача 8: Да се напише функция modifyComplexPotion (работи по същия начин като modify)
modifyComplexPotion :: ComplexPotion -> String -> Effect -> ComplexPotion
modifyComplexPotion c s e = map (map (modify s e)) c