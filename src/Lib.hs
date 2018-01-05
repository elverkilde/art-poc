{-|
Module      : Lib
Description : Lib's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Lib
    ( someFunc
    , Tree (..)
    , copaths
    , setup

    -- for debugging
    , publish
    ) where

import Lib.Prelude

-- Tree.hs imports

-- Setup.hs imports
import Crypto.Random.Types (MonadRandom)
import Crypto.PubKey.Curve25519

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


-- Should go to Setup.hs

keyExchange = flip dh

publish :: (MonadRandom m) => m [PublicKey]
publish = replicateM 2 (fmap toPublic generateSecretKey)

setup :: (MonadRandom m, MonadIO m) => [PublicKey] -> m ()
setup bundles = do
  -- secretKey <- generateSecretKey
  setupKey  <- generateSecretKey

  let leafKeys = fmap (keyExchange setupKey) bundles
  print (fmap secretKey leafKeys)
