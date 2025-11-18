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

-- Задача 1: twinPrimes 
twinPrimes :: [(Int, Int)]
twinPrimes = helper 3 5
    where 
        helper :: Int -> Int -> [(Int, Int)]
        helper x y = if isPrime x && isPrime y then (x, y) : helper (x+1) (y+1)
                    else helper (x+1) (y+1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = helper x (div x 2)
    where 
        helper :: Int -> Int -> Bool
        helper _ 1 = True
        helper n m = if mod n m == 0 then False else helper n (m-1)

-- Задача 2: Да се дефинира тип Coin
data Coin = Heads | Tails
    deriving (Show, Eq)

-- а) flipCoin
flipCoin :: Coin -> Coin
flipCoin Heads = Tails
flipCoin Tails = Heads

-- б) flipNCoin
flipNCoin :: Int -> Coin -> Coin
flipNCoin n c = if even n then c else flipCoin c

-- в) coinFlips 
coinFlips :: Coin -> [Coin]
coinFlips c = c : coinFlips (flipCoin c)

-- г) minFlips
minFlips :: [Coin] -> Int
minFlips cs = length $ filter (==Tails) $ take headsCount cs
    where 
        headsCount = length $ filter (==Heads) cs

-- д) coinField
coinField :: [Coin] -> Maybe (Int, Coin)
coinField [] = Nothing
coinField cs = Just $ helper cs (0, head cs) 
    where
        helper :: [Coin] -> (Int, Coin) -> (Int, Coin)  
        helper [] m = m
        helper (x : xs) (c, f) = if c > curr then helper xs (c, f) else helper xs (curr, x)
            where curr = 1 + length (takeWhile (==x) xs)


-- Задача 3: Да се дефинира тип Song. Да се дефинира тип Playlist
data Song = Song{name :: String, artist :: String, genre :: String, dur :: Int}
type Playlist = [Song]

songs :: Playlist
songs = [ Song{name = "a", artist = "1", genre = "a1", dur = 5},
          Song{name = "b", artist = "1", genre = "b1", dur = 10},
          Song{name = "c", artist = "2", genre = "c2", dur = 15},
          Song{name = "d", artist = "2", genre = "c2", dur = 20} ]

-- а) duration
duration :: Playlist -> Int 
duration ps = sum $ map dur ps

-- б) shorterThan
shorterThan :: Int -> Playlist -> Playlist
shorterThan n = filter (\x -> dur x < n)

-- в) artists
unique :: [String] -> [String]
unique [] = []
unique (x : xs) = if x `elem` xs then unique xs else x : unique xs

artists :: Playlist -> [String] 
artists ps = unique $ map artist ps

-- г) mostPopularGenre
mostPopularGenre :: Playlist -> [String]
mostPopularGenre ps = map name $ filter (\s -> genre s `elem` maxGenres) ps
    where 
        allGenres = unique $ map genre ps
        getGenreCount :: [String] -> [(Int, String)]
        getGenreCount [] = []
        getGenreCount (x : xs) = (length $ filter (\p -> genre p == x) ps, x) : getGenreCount xs 
        m = maximum $ map fst $ getGenreCount allGenres
        maxGenres = map snd $ filter (\(n, _) -> n == m) (getGenreCount allGenres)
