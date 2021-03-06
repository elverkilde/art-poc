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
import           Control.Monad.Random.Class
--import Crypto.Random.Types (MonadRandom)
--import Crypto.PubKey.Curve25519

import qualified Crypto.KDF.HKDF as HKDF
import           Control.Monad.State
import           Data.Map.Strict (Map)
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

mkForrest []  = []
mkForrest [(i, x)] = [Leaf i x]
mkForrest ((i1,x1):(i2,x2):xs) = Node () (Leaf i1 x1) (Leaf i2 x2):mkForrest xs

chop [] = []
chop [x] = [x]
chop (x1:x2:xs) = chop (Node () x1 x2 : chop xs)

mkTree (i1,x1) (i2,x2) xs = case chop (mkForrest ((i1, x1):(i2,x2):xs)) of
              [t] -> t
              _   -> Node () (Leaf i1 x1) (Leaf i2 x2)

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
  { leafKeys :: Map UserId DHSimplePub
  , copath   :: Map UserId [DHSimplePub]
  , setupKey :: DHSimplePub
  } deriving (Show, Eq)

setup :: (UserId, DHSimple) -> (UserId, DHSimplePub) -> [(UserId, DHSimplePub)] -> ARTGroup
setup creator m1 rest =
  let suk   = DHSimple 2
      leafs = (\(i, s) -> (i, exchange suk s)) <$> rest
      m1l   = (\(i, s) -> (i, exchange suk s)) $ m1
      tree  = mkTree creator m1l leafs
      nodes = mkNodes tree
      paths = Map.map (fmap getPub) $ Map.fromList $ coPaths nodes []
      pleafs = Map.map getPub $ Map.fromList $ m1l:leafs
  in ARTGroup pleafs paths (getPub suk)

getVal :: Tree a a -> a
getVal (Node v _ _) = v
getVal (Leaf _ v) = v

coPaths :: Tree a a -> [a] -> [(UserId, [a])]
coPaths (Node _ l r) acc = (coPaths l (getVal r:acc)) ++ (coPaths r (getVal l:acc))
coPaths (Leaf i _) acc = [(i, acc)]

chat2 :: ARTGroup
chat2 = let alice = ("alice", DHSimple 4)
            bob   = ("bob", getPub (DHSimple 3))
            eve   = ("eve", getPub (DHSimple 5))
            art   = ("art", getPub (DHSimple 6))
            group = setup alice bob [eve, art]
        in group

deriveTreeKey :: (UserId, DHSimple) -> ARTGroup -> Maybe DHSimple
deriveTreeKey (name, secret) (ARTGroup ls cPaths suk) =
    let leafKey = exchange secret suk
        coPath = Map.lookup name cPaths
    in (foldl exchange leafKey) <$> coPath

deriveStageKey stageKey treeKey = HKDF.extract stageKey treeKey
