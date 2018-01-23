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
data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving (Show, Eq)

instance (Monoid a) => Monoid (Tree a) where
  mempty = Leaf mempty
  mappend = Node

instance Foldable Tree where
  foldMap f (Leaf v) = f v
  foldMap f (Node t1 t2) = foldMap f t1 <> foldMap f t2

copaths :: (Monoid a) => (Tree a, [a]) -> [(a, [a])]
copaths (Leaf v, acc) = [(v, acc)]
copaths (Node l r, acc) = (copaths (l, merge r:acc))
                          ++ (copaths (r, merge l:acc))

merge :: (Monoid a) => Tree a -> a
merge = foldr (<>) mempty


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

type UserId = Text

chat :: State (Map UserId DHSimplePub) ()
chat = do
  let alice = DHSimple 4
      bob   = DHSimple 3

  modify $ Map.insert "alice" (getPub alice)
  modify $ Map.insert "bob" (getPub bob)

data CoPath = CoPath
  { trees :: [Tree UserId] }
  deriving (Show, Eq)

data ARTGroup = ARTGroup
  { leafKeys :: [DHSimplePub]
  , copath   :: Map UserId CoPath
  } deriving (Show, Eq)

setup :: DHSimple -> [DHSimplePub] -> ARTGroup
setup creator others =
  let suk = DHSimple 2
      leafs = (exchange suk) <$> others
      paths = Map.fromList []
  in ARTGroup (getPub <$> leafs) paths

chat2 = let alice = DHSimple 4
            bob   = DHSimple 3
            eve   = DHSimple 5
            jon   = DHSimple 6
            group = setup alice (getPub <$> [bob, eve, jon])
        in group

--keyExchange :: SecretKey -> PublicKey -> DhSecret
--keyExchange = flip dh

--publish :: (MonadRandom m) => m [PublicKey]
--publish = replicateM 2 (fmap toPublic generateSecretKey)

{-
setup :: (MonadRandom m, MonadIO m) => [PublicKey] -> m ()
setup bundles = do
  -- secretKey <- generateSecretKey
  setupKey  <- generateSecretKey

  let leafKeys = fmap (keyExchange setupKey) bundles
  print (fmap secretKey leafKeys)
-}
