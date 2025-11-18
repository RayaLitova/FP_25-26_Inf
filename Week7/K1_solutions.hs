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

-- Задача 1: twinPrimes 
twinPrimes :: [(Int, Int)]
twinPrimes = undefined

-- Задача 2: Да се дефинира тип Coin
-- а) flipCoin
-- б) flipNCoin
-- в) coinFlips 
-- г) minFlips
-- д) coinField

-- Задача 3: Да се дефинира тип Song. Да се дефинира тип Playlist
--songs :: Playlist
--songs = [ Song{name = "a", artist = "1", genre = "a1", dur = 5},
--          Song{name = "b", artist = "1", genre = "b1", dur = 10},
--          Song{name = "c", artist = "2", genre = "c2", dur = 15},
--          Song{name = "d", artist = "2", genre = "c2", dur = 20} ]

-- а) duration
-- б) shorterThan
-- в) artists
-- г) mostPopularGenre
