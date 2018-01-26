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
data Tree a b = Leaf a | Node b (Tree a b) (Tree a b)
            deriving (Show, Eq)

mkNodes n@(Node _ l r) = Node (merge n) (mkNodes l) (mkNodes r)
mkNodes (Leaf v) = Leaf v

merge (Node _ l r) = exchange1 (merge l) (merge r)
merge (Leaf v) = v

test = mkNodes (Node ()
                 (Node () (Leaf (DHSimple 5)) (Leaf (DHSimple 2)))
                 (Node () (Leaf (DHSimple 3)) (Leaf (DHSimple 4)))
               )

treeMap f (Node v l r) = Node (f v) (treeMap f l) (treeMap f r)
treeMap f (Leaf v) = Leaf (f v)

mkForrest []  = []
mkForrest [x] = [Leaf x]
mkForrest (x1:x2:xs) = Node () (Leaf x1) (Leaf x2):mkForrest xs

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
  { leafKeys :: [DHSimplePub]
  , copath   :: Map UserId [DHSimplePub]
  } deriving (Show, Eq)

setup :: DHSimple -> [DHSimplePub] -> ARTGroup
setup creator others =
  let suk = DHSimple 2
      leafs = (exchange suk) <$> others
      tree  = mkTree (creator:leafs)
      nodes = mkNodes <$> tree
      paths = Map.fromList []
  in ARTGroup (getPub <$> leafs) paths

getVal (Node v _ _) = v
getVal (Leaf v) = v

leafLocs1 (Node v l r) acc = Node () (leafLocs1 l (getVal r:acc)) (leafLocs1 r (getVal l:acc))
leafLocs1 (Leaf v) acc = Leaf acc

chat2 = let alice = DHSimple 4
            bob   = DHSimple 3
            eve   = DHSimple 5
            jon   = DHSimple 6
            group = setup alice (getPub <$> [bob, eve, jon])
        in group
