module Lib (
    headLensS,
    headLensT,
    headMaybeTraversal,
) where

import Control.Lens (
    Setter,
    Traversal,
    Traversal',
    sets,
 )
import Relude

headLensS :: Setter [a] (NonEmpty a) (Maybe a) a
headLensS = sets go
  where
    go :: (Maybe a -> a) -> [a] -> NonEmpty a
    go f [] = f Nothing :| []
    go f (x : xs) = f (Just x) :| xs

headLensT :: Traversal [a] (NonEmpty a) (Maybe a) a
headLensT f = go
  where
    go [] = (:| []) <$> f Nothing
    go (x : xs) = (:| xs) <$> f (Just x)

{- | This incorrect traversal traverses a list head and can set or replace the
 - head.
 -
 - This traversal is incorrect, because traversals shouldn't change the
 - number of elements in a structure they traverse.
-}
headMaybeTraversal :: Traversal' [a] (Maybe a)
headMaybeTraversal f = go
  where
    append xs Nothing = xs
    append xs (Just h) = h : xs

    go [] = append [] <$> f Nothing
    go (x : xs) = append xs <$> f (Just x)
