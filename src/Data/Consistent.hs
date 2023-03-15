-- | A simple data structure helping us ask questions of the following
-- sort: "does all this data have the same <BLANK> and if so what is
-- it?"
--
-- For example:
-- > doTheseHaveTheSameLength :: [String] -> String
-- > doTheseHaveTheSameLength l = case foldMap (Somebody . length) of
-- >   Somebody n -> "They all have length " <> show n
-- >   Nobody     -> "The lengths differ"
-- >   Anybody    -> "You didn't give me any strings"
module Data.Consistent (
  Consistent(..),
  getSomebody,
  ) where

-- | We have the following constructors:
-- * `Somebody` is a consistent choice of an element.
-- * `Nobody` is an inconsistent choice.
-- * `Anybody` is a failure to choose any element.
data Consistent a = Anybody | Somebody a | Nobody

-- | This picks out consistent choices as `Just`.
getSomebody :: Consistent a -> Maybe a
getSomebody (Somebody x) = Just x
getSomebody _ = Nothing

instance Functor Consistent where
  fmap _ Anybody = Anybody
  fmap f (Somebody x) = Somebody (f x)
  fmap _ Nobody = Nobody

instance (Eq a) => Semigroup (Consistent a) where
  Anybody <> x = x
  Nobody <> _ = Nobody
  Somebody x <> Anybody = Somebody x
  Somebody _ <> Nobody = Nobody
  Somebody x <> Somebody y
    | x == y = Somebody x
    | otherwise = Nobody

instance (Eq a) => Monoid (Consistent a) where
  mempty = Anybody
  mappend = (<>)
