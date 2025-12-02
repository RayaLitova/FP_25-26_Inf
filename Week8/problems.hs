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
{-# HLINT ignore "Use concatMap" #-}

-- Стандартни класове в Haskell:
    -- Functor - дефинира fmap
        -- class Functor f where
        --      fmap :: (a -> b) -> f a -> f b

    -- Applicative - functor + pure и <*> (ap or apply)
        -- class Functor f => Applicative f where
        --    pure :: a -> f a
        --    (<*>) :: f (a -> b) -> f a -> f b

    -- Monad - applicative + return и (>>=) (bind)
        --class Applicative m => Monad m where
        --    (>>=) :: m a -> (a -> m b) -> m b
        --    return :: a -> m a

-- Задача 1: Да се дефинира клас Tree със следните функции:
    -- empty - създава празно дърво
    -- insert - добавя елемент в дървото
    -- size - връща броя на node-овете в дървото
    -- isEmpty - проверява дали дървото е празно
    -- height - връща височината на дървото
    -- fromList
    -- toList
    -- contains - проверява дали елемент се съдържа в дървото
    -- mapTree
    -- foldTree

data Dir = L | R
    deriving Eq

class Tree t where
    empty :: t a
    insert :: t a -> a -> t a
    insertOrReplaceWithDir :: [Dir] -> t a -> a -> t a
    size :: t a -> Int
    size x = length $ toList x
    isEmpty :: t a -> Bool
    isEmpty x = size x == 0
    height :: t a -> Int
    fromList :: [a] -> t a
    toList :: t a -> [a]
    contains :: Eq a => t a -> a -> Bool
    contains x b = b `elem` toList x
    mapTree :: (a -> b) -> t a -> t b
    foldTree :: (b -> a -> b) -> b -> t a -> b

-- Задача 2: Да се инстанцира Tree за BTree и NormalTree
-- Задача 3: Да се инстанцира Show за BTree и NormalTree

data BTree а = Empty | Node а (BTree а) (BTree а)

tree :: BTree Int
tree = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 22 (Node 33 Empty Empty) (Node 44 Empty Empty))

tree2 :: BTree Int
tree2 = Node 1 (Node 2 (Node 4 Empty Empty) Empty) (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty))

instance Tree BTree where
    empty :: BTree a
    empty = Empty

    insert :: BTree a -> a -> BTree a
    insert Empty a = Node a Empty Empty
    insert (Node x l r) a = Node x (insert l a) r

    insertOrReplaceWithDir :: [Dir] -> BTree a -> a -> BTree a
    insertOrReplaceWithDir _ Empty a = Node a Empty Empty
    insertOrReplaceWithDir (x : xs) (Node a l r) b
        | x == R = Node a l (insertOrReplaceWithDir xs r b)
        | otherwise = Node a (insertOrReplaceWithDir xs l b) r
    insertOrReplaceWithDir [] (Node _ l r) a = Node a l r

    height :: BTree a -> Int
    height Empty = 0
    height (Node _ l r) = 1 + max (height l) (height r)

    fromList :: [a] -> BTree a
    fromList [] = Empty
    fromList (x : xs) = Node x (fromList a1) (fromList a2)
        where
            len = length xs `div` 2
            (a1, a2) = splitAt len xs

    toList :: BTree a -> [a]
    toList Empty = []
    toList (Node x l r) = x : toList l ++ toList r

    mapTree :: (a -> b) -> BTree a -> BTree b
    mapTree _ Empty =  Empty
    mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

    foldTree :: (b -> a -> b) -> b -> BTree a -> b
    foldTree _ b Empty = b
    foldTree f b (Node x l r) = f (foldTree f (foldTree f b l) r) x

instance Show a => Show (BTree a) where
    show :: Show a => BTree a -> String
    show t = unlines (line [t] : helper [t] [])
        where
            line :: [BTree a] -> String
            line [] = []
            line (Node x Empty Empty : xs) = show x ++ " " ++ line xs
            line (Node x l r : xs) = replicate (specialSize l) ' ' ++ show x ++ replicate (specialSize r + 1) ' ' ++ line xs
            line (Empty : xs) = ". " ++ line xs

            helper :: [BTree a] -> [BTree a] -> [String]
            helper [] [] = []
            helper [] l = line l : helper l []
            helper (Node _ Empty Empty : xs) n = helper xs n
            helper (Node _ l r : xs) n = helper xs (n ++ [l, r])
            helper (Empty : xs) n = helper xs n

            specialSize :: BTree a -> Int
            specialSize Empty = 1
            specialSize (Node _ Empty Empty) = 1
            specialSize (Node _ l r) = 1 + specialSize l + specialSize r


data NormalTree а = NormalEmpty | NormalNode а [NormalTree а]

instance Tree NormalTree where
    empty :: NormalTree a
    empty = NormalEmpty

    insert :: NormalTree a -> a -> NormalTree a
    insert NormalEmpty a = NormalNode a []
    insert (NormalNode b (x : xs)) a = NormalNode b (insert x a : xs)
    insert (NormalNode b []) a = NormalNode b [NormalNode a []]

    insertOrReplaceWithDir :: [Dir] -> NormalTree a -> a -> NormalTree a
    insertOrReplaceWithDir _ NormalEmpty a = NormalNode a []
    insertOrReplaceWithDir [] (NormalNode _ nodes) a = NormalNode a nodes
    insertOrReplaceWithDir _ (NormalNode x []) a = NormalNode x [NormalNode a []]
    insertOrReplaceWithDir (L : ds) (NormalNode x nodes) a = NormalNode x (insertOrReplaceWithDir ds (head nodes) a : tail nodes)
    insertOrReplaceWithDir (R : ds) (NormalNode x nodes) a = NormalNode x (insertOrReplaceWithDir ds (last nodes) a : take (length nodes - 1) nodes)

    height :: NormalTree a -> Int
    height NormalEmpty = 0
    height (NormalNode _ nodes) = 1 + maximum (map height nodes)

    fromList :: [a] -> NormalTree a
    fromList [] = NormalEmpty
    fromList (x : xs) = NormalNode x $ map (\n -> fromList [n]) xs

    toList :: NormalTree a -> [a]
    toList NormalEmpty = []
    toList (NormalNode x nodes) = x : helper nodes
        where 
            helper [] = []
            helper (n : ns) = toList n ++ helper ns

    mapTree :: (a -> b) -> NormalTree a -> NormalTree b
    mapTree _ NormalEmpty = NormalEmpty
    mapTree f (NormalNode x nodes) = NormalNode (f x) (map (mapTree f) nodes)

    foldTree :: (b -> a -> b) -> b -> NormalTree a -> b
    foldTree f b t = foldl f b $ toList t

normalTree :: NormalTree Int
normalTree = NormalNode 1 [NormalNode 2 [], NormalNode 3 [], NormalNode 4 [NormalNode 5 [NormalNode 6 []], NormalNode 7 []]]

-- Вариант с map
instance Show a => Show (NormalTree a) where
    show NormalEmpty = "Empty"
    show node = unlines $ helper 0 node
        where
            helper :: Int -> NormalTree a -> [String]
            helper _ NormalEmpty = []
            helper 0 (NormalNode x nodes) = show x : concat (map (helper 1) nodes)
            helper n (NormalNode x nodes) = (replicate (n-1) '|' ++ "-" ++ show x) : concat (map (helper (n+1)) nodes)

-- Вариант без map
--instance Show a => Show (NormalTree a) where
--    show :: Show a => NormalTree a -> String
--    show NormalEmpty = "Empty"
--    show (NormalNode v ns) = unlines $ show v : helper ns 1
--        where 
--            helper :: [NormalTree a] -> Int -> [String]
--            helper [] _ = []
--            helper (NormalEmpty : xs) c = helper xs c
--            helper (NormalNode x nodes : xs) c = (replicate (c-1) '|' ++ "-" ++ show x) : helper nodes (c+1) ++ helper xs c
