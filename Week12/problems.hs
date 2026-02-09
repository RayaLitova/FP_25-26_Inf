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
{-# HLINT ignore "Use mapMaybe" #-}
{-# HLINT ignore "Use list comprehension" #-}
import System.Random
import Data.List (nub)
import Data.Maybe (isNothing, isJust)
{-# HLINT ignore "Replace case with fromMaybe" #-}

-- Задача 1: Да се дефинира тип за судоку дъска
type SudokuBoard = [[Int]]

-- Задача 2: Да се напише функция, която при подадени ред и колона връща всички възможни числа, 
--           които могат да бъдат на тази позиция
getRow :: SudokuBoard -> Int -> [Int]
getRow s i = s !! i

transpose :: SudokuBoard -> SudokuBoard
transpose [] = []
transpose [[]] = []
transpose s = map head s : transpose (map tail s)

getColumn :: SudokuBoard -> Int -> [Int]
getColumn s i = transpose s !! i

getBlock :: SudokuBoard -> Int -> Int -> [Int]
getBlock s i j = [s !! x !! y | x <- [i - (i `mod` 3) .. (i - (i `mod` 3) + 2)], y <- [j - (j `mod` 3) .. (j - (j `mod` 3) + 2)]]

sudokuExample :: SudokuBoard -- 0,1 -> 7,9
sudokuExample =
    [
        [1,0,0,0,0,0,0,0,3],
        [2,3,0,0,0,0,0,0,6],
        [4,5,6,0,0,9,1,0,7],
        [0,0,0,0,0,8,6,0,0],
        [0,8,0,3,5,7,0,9,0],
        [0,0,4,2,0,0,0,0,0],
        [7,0,9,1,0,0,4,0,0],
        [8,0,0,0,0,0,5,6,0],
        [3,0,0,0,0,0,7,8,9]
    ]

sudokuResult :: SudokuBoard
sudokuResult = 
    [
        [1,9,7,5,6,2,8,4,3],
        [2,3,8,7,4,1,9,5,6],
        [4,5,6,8,3,9,1,2,7],
        [9,2,3,4,1,8,6,7,5],
        [6,8,1,3,5,7,2,9,4],
        [5,7,4,2,9,6,3,1,8],
        [7,6,9,1,8,5,4,3,2],
        [8,4,2,9,7,3,5,6,1],
        [3,1,5,6,2,4,7,8,9]
    ]

sudokuTest :: SudokuBoard
sudokuTest = 
    [
        [1,0,7,5,6,2,8,4,3],
        [2,3,8,7,4,1,9,5,6],
        [4,5,6,8,3,9,1,2,7],
        [9,2,3,4,1,8,6,7,5],
        [6,8,1,3,5,7,2,9,4],
        [5,7,4,2,9,6,3,1,8],
        [7,6,9,1,8,5,4,3,2],
        [8,4,2,9,7,3,5,6,1],
        [3,1,5,6,2,4,7,8,9]
    ]

union :: [Int] -> [Int] -> [Int]
union x y = filter (/= 0) $ nub $ x ++ y

complement :: [Int] -> [Int]
complement x = filter (`notElem` x) [1..9]

getOptions :: SudokuBoard -> Int -> Int -> [Int]
getOptions b x y = complement $ union (getBlock b x y) $ union (getRow b x) (getColumn b y)

-- Задача 3: Да се дефинира тип за дърво от судоку дъски
type Derivatives = [SudokuTree] 

data SudokuTree = Node SudokuBoard Derivatives
    deriving Show

-- Задача 4: Да се напише функция, която генерира дърво от судоку дъска. 
-- Коренът на дървото трябва да бъде оригиналната дъска
-- Започваме от клетка 0,0 и обхождаме по редове
-- Нека текущата позиция е (x, y). 
-- Нивото на дървото, на което се намираме в момента, трябва да съдържа всички варианти на дъската,
-- в които клетка (x, y) е запълнена с валидно число

changeBoard :: SudokuBoard -> Int -> Int -> Int -> SudokuBoard
changeBoard s x y n = take x s ++ [take y (s !! x) ++ [n] ++ drop (y + 1) (s !! x)] ++ drop (x + 1) s 

increaseIndex :: Int -> Int -> (Int, Int)
increaseIndex 8 8 = (-1, -1)
increaseIndex 8 y = (0, y + 1)
increaseIndex x y = (x + 1, y)

getCell :: SudokuBoard -> Int -> Int -> Int 
getCell s i j = s !! i !! j

generateSudokuTree :: SudokuBoard -> SudokuTree 
generateSudokuTree board = helper board 0 0
    where 
        helper :: SudokuBoard -> Int -> Int -> SudokuTree 
        helper s (-1) (-1) = Node s [] 
        helper s i j = 
            if getCell s i j == 0 then
                Node s $ map (\b -> helper (changeBoard s i j b) nx ny) (getOptions s i j)
            else 
                helper s nx ny
            where 
                (nx, ny) = increaseIndex i j
        
isSolved :: SudokuBoard -> Bool 
isSolved s = null $ filter (elem 0) s

traverseTree :: SudokuTree -> Maybe SudokuBoard
traverseTree (Node b []) = if isSolved b then Just b else Nothing 
traverseTree (Node _ o) = if null l then Nothing else head l
    where l = filter isJust $ map traverseTree o 

getSolution :: SudokuBoard -> SudokuBoard 
getSolution s = 
    let sol = traverseTree $ generateSudokuTree s 
    in 
        case sol of
            Just b -> b 
            Nothing -> [] 

checkSolution :: Bool 
checkSolution = getSolution sudokuExample == sudokuResult

sudokuGenTest :: SudokuBoard
sudokuGenTest = 
    [
        [1,0,7,0,6,0,0,4,0],
        [2,0,8,7,0,1,0,0,6],
        [0,0,6,0,3,9,1,0,7],
        [9,0,0,4,1,8,0,0,5],
        [6,0,1,0,5,7,2,9,4],
        [5,0,4,0,0,6,0,0,0],
        [0,6,9,0,8,0,0,3,2],
        [0,4,0,9,0,3,0,6,1],
        [3,0,5,6,0,0,0,0,0]
    ]

hasSingleSolution :: SudokuBoard -> Bool 
hasSingleSolution s = length (getAllSolutions (generateSudokuTree s)) == 1

getAllSolutions :: SudokuTree -> [SudokuBoard]
getAllSolutions (Node b []) = if isSolved b then [b] else [] 
getAllSolutions (Node _ o) = concat l
    where l = map getAllSolutions o 

-- Задача 5: Да се дефинира функция, която обхожда дървото и връща първата срещната запълнена дъска,
-- ако има такава
-- Задача 6: Да се дефинира функция, която приема дъска за судоку и връща нейното решение

generateEmpty :: SudokuBoard 
generateEmpty = replicate 9 $ replicate 9 0

generateSudokuBoard :: SudokuBoard -> Int -> IO ()
generateSudokuBoard b n = do
    if n == 0 then
        print $ hasSingleSolution b
    else do
        l <- randomRIO (0, 16) :: IO Int 
        let is = [(x, y) | x <- [0 .. l], y <- [0 .. l], x + y == l, x <= 8, y <= 8]
        let filtered = filter (\(x, y) -> getCell b x y == 0) is
        if null filtered then
            generateSudokuBoard b n 
        else do
            let (x, y) = head filtered
            let options = getOptions b x y 
            if null options then 
                generateSudokuBoard b n
            else do
                i <- randomRIO (0, length options - 1) :: IO Int 
                generateSudokuBoard (changeBoard b x y (options !! i)) (n - 1)