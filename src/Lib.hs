module Lib
  ( headLensS,
    headLensT,
  )
where

import Control.Lens
  ( Setter,
    Traversal,
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
