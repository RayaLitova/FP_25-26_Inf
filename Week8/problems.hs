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

-- Задача 2: Да се инстанцира Tree за BTree и NormalTree
-- Задача 3: Да се инстанцира Show за BTree и NormalTree

data BTree а = Empty | Node а (BTree а) (BTree а)

tree :: BTree Int
tree = Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) (Node 22 (Node 33 Empty Empty) (Node 44 Empty Empty))

data NormalTree а = NormalEmpty | NormalNode а [NormalTree а]

normalTree :: NormalTree Int
normalTree = NormalNode 1 [NormalNode 2 [], NormalNode 3 [], NormalNode 4 [NormalNode 5 [NormalNode 6 []], NormalNode 7 []]]
