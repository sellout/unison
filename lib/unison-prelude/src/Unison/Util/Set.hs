module Unison.Util.Set
  ( asSingleton,
    difference1,
    intersects,
    mapMaybe,
    Unison.Util.Set.traverse,
    flatMap,
  )
where

import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set

-- | Get the only member of a set, iff it's a singleton.
asSingleton :: Set a -> Maybe a
asSingleton xs =
  if Set.size xs == 1 then Just (Set.findMin xs) else Nothing

-- | Set difference, but return @Nothing@ if the difference is empty.
difference1 :: (Ord a) => Set a -> Set a -> Maybe (Set a)
difference1 xs ys =
  if null zs then Nothing else Just zs
  where
    zs = Set.difference xs ys

-- | Get whether two sets intersect.
intersects :: (Ord a) => Set a -> Set a -> Bool
intersects xs ys =
  not (Set.disjoint xs ys)

mapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybe f = Set.fromList . Maybe.mapMaybe f . Set.toList

traverse :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverse f = fmap Set.fromList . Prelude.traverse f . Set.toList

flatMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
flatMap f = Set.unions . fmap f . Set.toList
