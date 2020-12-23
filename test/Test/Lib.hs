{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}

module Test.Lib
  ( tests,
  )
where

import Control.Lens
  ( ASetter,
    Traversal,
    set,
  )
import Lib
  ( headLensS,
    headLensT,
  )
import Relude
import Test.Hspec
  ( SpecWith,
    describe,
    it,
  )
import Test.QuickCheck (property)

tests :: SpecWith ()
tests = do
  describe "Sets a head on an empty list" $ do
    it "headLensS satisfies" $ do
      property $ prop_setsHeadOnAnEmptyList @Int headLensS
    it "headLensT satisfies" $ do
      property $ prop_setsHeadOnAnEmptyList @Int headLensS
  describe "Sets a head on a list" $ do
    it "headLensS satisfies" $ do
      property $ prop_setsHeadOnAList @Int headLensS
    it "headLensT satisfies" $ do
      property $ prop_setsHeadOnAList @Int headLensS
  describe "Keeps the tail on a list" $ do
    it "headLensS satisfies" $ do
      property $ prop_keepsTailOnAList @Int headLensS
    it "headLensT satisfies" $ do
      property $ prop_keepsTailOnAList @Int headLensS
  describe "second set wins lens like law" $ do
    it "headLensS satisfies" $ do
      property $ prop_secondSetWins @Int headLensS
    it "headLensT satisfies" $ do
      property $ prop_secondSetWins @Int headLensT

prop_setsHeadOnAnEmptyList ::
  (Eq x) =>
  ASetter [x] (NonEmpty x) b x ->
  x ->
  Bool
prop_setsHeadOnAnEmptyList lens x =
  set lens x [] == [x]

prop_setsHeadOnAList ::
  (Eq x) =>
  ASetter [x] (NonEmpty x) b x ->
  [x] ->
  x ->
  Bool
prop_setsHeadOnAList lens xs x =
  head modifiedXs == x
  where
    modifiedXs = set lens x xs

prop_keepsTailOnAList ::
  (Eq x) =>
  ASetter [x] (NonEmpty x) b x ->
  [x] ->
  x ->
  Bool
prop_keepsTailOnAList lens xs x =
  tail modifiedXs == expectedTail
  where
    modifiedXs = set lens x xs
    expectedTail = case xs of
      [] -> []
      (_ : ys) -> ys

-- The only law mentioned at
-- https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Setter.html#g:1
prop_secondSetWins ::
  (Eq x) =>
  ASetter [x] (NonEmpty x) b x ->
  [x] ->
  x ->
  x ->
  Bool
prop_secondSetWins lens xs x y =
  set lens y (toList $ set lens x xs) == set lens y xs

-- The laws mentioned at
-- https://hackage.haskell.org/package/lens-4.19.2/docs/Control-Lens-Traversal.html
prop_firstTraversalLaw ::
  (Eq x) =>
  Traversal [x] [x] x x ->
  [x] ->
  Bool
prop_firstTraversalLaw lens xs =
  lens Identity xs == Identity xs

prop_secondTraversalLaw ::
  (Applicative f, Eq (f (f [x]))) =>
  Traversal [x] [x] x x ->
  [x] ->
  (x -> f x) ->
  (x -> f x) ->
  Bool
prop_secondTraversalLaw lens xs f g =
  (fmap (lens f) . lens g) xs == (getCompose . lens (Compose . fmap f . g)) xs
