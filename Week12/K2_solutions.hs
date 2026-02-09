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
{-# HLINT ignore "Use newtype instead of data" #-}

data Btree a = Empty | Node a (Btree a) (Btree a)
    deriving Show 

generateInfTree :: (a -> (a, a)) -> a -> Btree a 
generateInfTree f a = Node a (generateInfTree f l) (generateInfTree f r)
    where (l,r) = f a

takeDepth :: Btree a -> Int -> Btree a
takeDepth Empty _ = Empty
takeDepth _ 0 = Empty
takeDepth (Node a l r) n = Node a (takeDepth l (n-1)) (takeDepth r (n-1))

natTree :: Btree Int 
natTree = generateInfTree (\x -> (2*x, 2*x + 1)) 1

instance Functor Btree where 
    fmap :: (a -> b) -> Btree a -> Btree b 
    fmap _ Empty = Empty
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

data Hallway = End | Branch [Hallway] | Door Int Hallway 
    deriving (Show, Eq) 
data OfficeLayout = Entrance Hallway
    deriving (Show, Eq)

workersNeeded :: OfficeLayout -> [Int]
workersNeeded (Entrance hallway) = helper hallway
    where 
        helper :: Hallway -> [Int]
        helper End = []
        helper (Door n h) = n : helper h 
        helper (Branch hs) = concatMap helper hs 


officeExample :: OfficeLayout
officeExample = Entrance (Door 5 (Door 2 (Branch [Door 3 End, Door 2 (Door 2 End), Door 2 End])))

officeExample2 :: OfficeLayout
officeExample2 = Entrance (Door 5 (Door 2 (Branch [Door 3 End, Door 2 (Door 2 End), Branch [Door 2 End, Door 2 (Door 3 End)]])))

branchLoads :: OfficeLayout -> [[Int]]
branchLoads (Entrance hallway) = helper hallway 
    where 
        helper :: Hallway -> [[Int]]
        helper End = []
        helper (Door _ h) = helper h 
        helper (Branch hs) = map (sum . workersNeeded . Entrance) hs : concatMap helper hs 

optimize :: OfficeLayout -> OfficeLayout
optimize (Entrance hallway) = Entrance $ helper hallway 
    where 
        helper :: Hallway -> Hallway 
        helper End = End 
        helper (Door n (Door m h)) = helper (Door (n+m) h)
        helper (Door n h) = Door n $ helper h
        helper (Branch hs) = Branch $ map helper hs