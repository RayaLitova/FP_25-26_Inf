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

import Data.List (nub, sort)

type Graph a = ([a], [(a, a)])

-- Задача 1: Да се дефинират типове за неориентиран, ориентиран и тегловен граф

-- Задачи с неориентирани графи:

-- Задача 1: Да се напише функция, която връща списък от всички свързани компоненти в граф
isAdjacent :: Eq a => Graph a -> a -> a -> Bool   
isAdjacent (_, es) x y = (x, y) `elem` es || (y,x) `elem` es

getNeighbours :: Eq a => Graph a -> a -> [a]
getNeighbours (_, es) x = map (\(a, b) -> if x == a then b else a) $ filter (\(a, b) -> x == a || x == b) es

hasPath :: Eq a => Graph a -> a -> a -> Bool
hasPath gr x y = y `elem` helper gr [x] [] 
    where 
        helper :: Eq a => Graph a -> [a] -> [a] -> [a]
        helper _ [] v = v
        helper g c v = helper g nc nv
            where 
                nc = filter (\a -> not (a `elem` v)) $ concat $ map (getNeighbours g) c
                nv = c ++ v

getConnectedComponent :: Eq a => Graph a -> a -> [a]
getConnectedComponent gr x = helper gr [x] [] 
    where 
        helper :: Eq a => Graph a -> [a] -> [a] -> [a]
        helper _ [] v = v
        helper g c v = helper g nc nv
            where 
                nc = filter (\a -> not (a `elem` v)) $ concat $ map (getNeighbours g) c
                nv = c ++ v

getAllConnectedComponents :: (Eq a, Ord a) => Graph a -> [[a]]
getAllConnectedComponents g@(vs, _) = nub $ map (sort . getConnectedComponent g) vs

-- Задача 2: Да се напише функция, която проверява дали граф е Ойлеров
isEulerGraph :: (Eq a, Ord a) => Graph a -> Bool
isEulerGraph g@(vs, _) = length (getAllConnectedComponents g) == 1 && all even (map (length . getNeighbours g) vs) 


type OrientedGraph a = ([a], [(a, a)])
-- Задачи с ориентирани графи:
-- Задача 1: Да се напише функция, която приема граф и връща наредена тройка от върха, степен на входа и степен на изхода
getDegrees :: Eq a => OrientedGraph a -> [(a, Int, Int)]
getDegrees (vs, es) = map (\x -> (x, length $ filter (\e -> fst e == x) es, length $ filter (\e -> snd e == x) es)) vs

-- Задача 2: Да се напише функция, която приема граф и обръща всички ребра
reverseGraph :: Eq a => OrientedGraph a -> OrientedGraph a
reverseGraph (vs, es) = (vs, map (\(x, y) -> (y, x)) es)

-- Задача 3: Да се намерят всички цикли в граф
getNeighboursO :: Eq a => Graph a -> a -> [a]
getNeighboursO (_, es) x = map snd $ filter (\(a, _) -> x == a) es



