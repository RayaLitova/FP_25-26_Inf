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

-- Задача 1: Да се дефинират типове Suit и Rank
data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Enum, Eq, Ord)

type Rank1 = Int
data Rank2 = Ace | King | Queen | Jack | Number Int
    deriving (Show, Eq)

-- Задача 2: Да се дефинира тип Card, който има следните полета: rank :: Rank, suit :: Suit
data Card = Card {rank :: Rank2, suit :: Suit}
    deriving Eq

-- Това не го гледайте
instance Show Card where
    show c = " " ++ show (rank c) ++ " of " ++ show (suit c)
-- 

intToRank :: Int -> Rank2
intToRank 14 = Ace
intToRank 13 = King
intToRank 12 = Queen
intToRank 11 = Jack
intToRank x = if x > 14 || x < 2 then error "Invalid" else Number x

rankToInt :: Rank2 -> Int
rankToInt Ace = 14
rankToInt King = 13
rankToInt Queen = 12
rankToInt Jack = 11
rankToInt (Number x) = x

-- Задача 3: Да се напише функция, която генерира стандартно тесте от 52 карти
fullDeck :: [Card]
fullDeck = [Card { rank = intToRank rank, suit = suit } | rank <- [2 .. 14], suit <- [Clubs .. ]]

-- Задача 3: Да се дефинира тип Player, който има следните полета: name :: String, age :: Int, Hand :: [Card]
data Player = Player {name :: String, age :: Int, hand :: [Card]}
    deriving (Eq)

-- Това също не го гледайте
instance Show Player where
    show p = "\nPlayer " ++ name p ++ ", age " ++ show (age p) ++ ", hand: " ++ show (hand p)
--

players :: [Player]
players = [Player{name = "p1", age = 20, hand = []},
            Player{name = "p2", age = 20, hand = []},
            Player{name = "p3", age = 20, hand = []},
            Player{name = "p4", age = 20, hand = []}]

-- Задача 4: Да се напише функция, която приема списък от играчи и им раздава всички карти последователно

-- Вариант 1
dealCards :: [Player] -> [Card] -> [Player]
dealCards ps [] = ps
dealCards (p : ps) (c : cs) = dealCards (ps ++ [Player{name = name p, age = age p, hand = c : hand p}]) cs
dealCards [] _ = error "oops"

-- Вариант 2
giveCard :: Player -> Card -> Player
giveCard p c = Player {name = name p, age = age p, hand = c : hand p }

dealCards2 :: [Player] -> [Card] -> [Player]
dealCards2 ps [] = ps
dealCards2 ps cs = dealCards (zipWith giveCard ps $ take (length ps) cs) $ drop (length ps) cs

dealFullDeck :: [Player] -> [Player]
dealFullDeck ps = dealCards ps fullDeck

-- Задача 5: Да се напише функция, която приема списък от играчи и връща името на играча с най-висока сума от ранговете на картите в ръката му
highestSum :: [Player] -> String
highestSum l = helper l 0 (head l)
    where
        helper :: [Player] -> Int -> Player -> String
        helper [] _ p = name p
        helper (p : ps) c cp = if s > c then helper ps s p else helper ps c cp
            where s = sum $ map (rankToInt . rank) $ hand p

-- Задача 6: Да се напише функция, която приема играч и връща всички червени карти от ръката му

red :: [Suit]
red = [Hearts, Diamonds]

getAllReds :: Player -> Int
getAllReds p = sum $ map ((\s -> if s `elem` red then 1 else 0) . suit) $ hand p
--                              if s == Hearts || s == Diamonds

-- Задача 7: Да се напише функция, която генерира стандартно тесте за игра на белот
belotDeck :: [Card]
belotDeck = [Card { rank = intToRank rank, suit = suit } | rank <- [7 .. 14], suit <- [Clubs .. ]]

-- Задача 8: Да се напише функция, която цепи тестето на подадено от играча място
cutDeck :: [Card] -> Int -> [Card]
cutDeck d n = drop n d ++ take n d

-- Задача 9: Да се напише функция, която цепи тестето n пъти. Изберете произволен pattern, с който да направите безкрайния списък, от който се избира място за цепене
pattern :: [Int]
pattern = [1 .. 32] ++ [32, 16 .. 2] ++ [32, 16 .. 2]

cutDeckNTimes :: [Card] -> Int -> [Card]
cutDeckNTimes cards times = helper cards times pattern
    where
        helper :: [Card] -> Int -> [Int] -> [Card]
        helper cs 0 _ = cs
        helper cs n (m : ms) = helper (cutDeck cs m) (n-1) ms
        helper cs n [] = helper cs n pattern


-- Задача 10: Да се напише функция, която раздава карти (приемете, че веднага е имало обявяване)
giveNCardsToPlayers :: [Card] -> Int -> [Player] -> [Player]
giveNCardsToPlayers cs n (p : ps) = Player {name = name p, age = age p, hand = take n cs ++ hand p } : giveNCardsToPlayers (drop n cs) n ps   
giveNCardsToPlayers _ _ [] = []

giveBelotCards :: [Card] -> [Player] -> [Player]
giveBelotCards cs ps = giveNCardsToPlayers (drop (drop2 + drop3) cs) 3 $ giveNCardsToPlayers (drop drop3 cs) 2 $ giveNCardsToPlayers cs 3 ps
    where 
        drop3 = 3 * length ps
        drop2 = 2 * length ps
        
-- Задача 11: Да се напише функция, която приема карта и играч и избира коя карта от ръката му да играе 
    -- По възможност трябва да отговаря на боя и да е с по-висок ранк
    -- Ако играчът няма възможност да даде карта с по-висок ранк от същата боя, трябва да даде картата с най-нисък ранк от тази боя
    -- Ако играчът няма възможност да даде карта от същата боя, трябва да даде картата с най-нисък ранк в ръката си
    -- Ако има повече от една карта с най-нисък ранк да приоризира боите както в стандартна игра на белот

getHighestCard :: [Card] -> Card -> Card
getHighestCard [] c = c
getHighestCard (c : cs) m = if rankToInt (rank c) > rankToInt (rank m) then getHighestCard cs c else getHighestCard cs m

getLowestCard :: [Card] -> Card -> Card
getLowestCard [] c = c
getLowestCard (c : cs) m
  | rankToInt (rank c) < rankToInt (rank m) = getLowestCard cs c
  | rankToInt (rank c) > rankToInt (rank m) = getLowestCard cs m
  | suit c > suit m = getLowestCard cs c
  | otherwise = getLowestCard cs m

selectCard :: Player -> Card -> Card
selectCard p c =
    let l = filter (\x -> suit x == suit c) (hand p)
        h = getHighestCard l c
    in
        if not $ null l then
            if h /= c then h
            else getLowestCard l c
        else getLowestCard (hand p) (head $ hand p)


-- Задача 12: Да се напише функция, която преценя кой играч е взел ръката. Предполагаме, че първия играч в списъка е бил под ръка
currHand :: [Card]
currHand = [Card { suit = Hearts, rank = Jack }, 
            Card { suit = Spades, rank = Number 7 }, 
            Card { suit = Hearts, rank = Number 10 }, 
            Card { suit = Hearts, rank = Queen }]

whoWon :: [Player] -> [Card] -> Player
whoWon ps cs = getPlayer (getHighestCard (map snd cards) (head cs)) cards
    where 
        cards = filter (\(_, x) -> suit x == suit (head cs)) $ zip ps cs 
        getPlayer :: Card -> [(Player, Card)] -> Player
        getPlayer x ((p, y) : l) = if x == y then p else getPlayer x l
        getPlayer _ [] = error "oops"

-- Задача 13: Да се напише функция, която према ръка и играчите и размества списъка така, че взелият играч да е под ръка
nextTurn :: [Player] -> [Card] -> [Player]
nextTurn ps cs = dropWhile (/= p) ps ++ takeWhile (/= p) ps
    where p = whoWon ps cs

