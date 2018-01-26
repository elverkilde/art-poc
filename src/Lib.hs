{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
{-    ( someFunc
    , Tree (..)
    , copaths
    , setup

    -- for debugging
    , publish
    )
-}
where

import Lib.Prelude

-- Tree.hs imports

-- Setup.hs imports
import Control.Monad.Random.Class
--import Crypto.Random.Types (MonadRandom)
--import Crypto.PubKey.Curve25519

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Prints someFunc
--
-- >>> someFunc 10
-- someFunc
someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: Text)


-- Should go to Tree.hs
data Tree a b = Leaf UserId a | Node b (Tree a b) (Tree a b)
            deriving (Show, Eq)

mkNodes n@(Node _ l r) = Node (merge n) (mkNodes l) (mkNodes r)
mkNodes (Leaf i v) = Leaf i v

merge (Node _ l r) = exchange1 (merge l) (merge r)
merge (Leaf i v) = v

test = mkNodes (Node ()
                 (Node () (Leaf "a" (DHSimple 5)) (Leaf "b" (DHSimple 2)))
                 (Node () (Leaf "c" (DHSimple 3)) (Leaf "d" (DHSimple 4)))
               )
{-
treeMap f (Node v l r) = Node (f v) (treeMap f l) (treeMap f r)
treeMap f (Leaf i v) = Leaf i (f v)
-}

mkForrest []  = []
mkForrest [(i, x)] = [Leaf i x]
mkForrest ((i1,x1):(i2,x2):xs) = Node () (Leaf i1 x1) (Leaf i2 x2):mkForrest xs

chop [] = []
chop [x] = [x]
chop (x1:x2:xs) = chop (Node () x1 x2 : chop xs)

mkTree xs = case chop (mkForrest xs) of
              [t] -> Just t
              _   -> Nothing

newtype DHSimple = DHSimple { private :: Int }
  deriving (Show, Eq)

newtype DHSimplePub = DHSimplePub { public :: Int }
  deriving (Show, Eq)

genSecret :: MonadRandom m => m DHSimple
genSecret = DHSimple <$> getRandom

p :: Int
p = 23
g :: Int
g = 5

getPub :: DHSimple -> DHSimplePub
getPub (DHSimple priv) = DHSimplePub $ (g ^ priv) `mod` p

exchange :: DHSimple -> DHSimplePub -> DHSimple
exchange (DHSimple priv) (DHSimplePub pub) =
  DHSimple $ (pub ^ priv) `mod` p

exchange1 :: DHSimple -> DHSimple -> DHSimple
exchange1 (DHSimple priv) p2 =
  let (DHSimplePub pub) = getPub p2
  in DHSimple $ (pub ^ priv) `mod` p

type UserId = Text


data ARTGroup = ARTGroup
  { leafKeys :: [(UserId, DHSimple)]
  , copath   :: Map UserId [DHSimple]
  } deriving (Show, Eq)

setup :: (UserId, DHSimple) -> [(UserId, DHSimplePub)] -> Maybe ARTGroup
setup creator others =
  let suk = DHSimple 2
      leafs = (\(i, s) -> (i, exchange suk s)) <$> others
      tree  = mkTree (creator:leafs)
      nodes = mkNodes <$> tree
      paths = Map.fromList <$> (flip leafLocs1 [] <$> nodes)
  in (ARTGroup leafs) <$> paths

getVal (Node v _ _) = v
getVal (Leaf i v) = v

leafLocs1 (Node v l r) acc = (leafLocs1 l (getVal r:acc)) ++ (leafLocs1 r (getVal l:acc))
leafLocs1 (Leaf i v) acc = [(i, acc)]

chat2 = let alice = ("alice", DHSimple 4)
            bob   = ("bob", getPub (DHSimple 3))
            eve   = ("eve", getPub (DHSimple 5))
            art   = ("art", getPub (DHSimple 6))
            group = setup alice [bob, eve, art]
        in group
