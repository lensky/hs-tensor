{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Permutation
       (
       -- * Main class and type
       Permutation
       ,Permutable(..) 
       -- * Permutation operations
       ,invert
       ,toList
       ,fromList
       ,unsafeFromList
       ,fromPairs
       ,toPairs
       -- * Useful permutations
       ,swap01
       -- * Utility functions 
       ,sortByListWithP
       ,sortListWithP)
where

import SetMappings (Subset(..))

import Data.List (sortBy)

import Data.Tuple (swap)

import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Function (on)

import qualified Data.IntMap as IM

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

-- | Type that holds instructions to 'permute' 'Permutable' objects.
newtype Permutation = Permutation { pIMap :: IM.IntMap Int } deriving (Eq)

instance Show Permutation where
  show p = "fromPairs " ++ show (toPairs p)

instance Monoid Permutation where
  mempty = Permutation IM.empty
  a `mappend` b = a `compose` b

instance Subset Permutation [Int] where
  toSuperSet p = map (\x -> fromMaybe x (IM.lookup x $ pIMap p)) [0..]
  unsafeFromSuperSet = Permutation . IM.fromDistinctAscList . convert . zip [0..]
    where convert = foldr ifsame []
            where ifsame (k,v) l = if k == v
                                      then l
                                      else (k,v):l

instance Subset Permutation [(Int,Int)] where
  toSuperSet = toPairs
  unsafeFromSuperSet = fromPairs

instance Subset Permutation (IM.IntMap Int) where
  toSuperSet = pIMap
  unsafeFromSuperSet = Permutation

-- | Every permutation has a canonical representation as a list
-- [p_1,...,p_n], where i gets mapped to p_i.
toList :: Permutation -> [Int]
toList = toSuperSet

-- | The inverse of 'toList'.
fromList :: [Int] -> Maybe Permutation
fromList = fromSuperSet

-- | Unsafe version of 'fromList'.
unsafeFromList :: [Int] -> Permutation
unsafeFromList = unsafeFromSuperSet

-- | What does the permutation map a given index to. Used as infix.
maps :: Permutation -> Int -> Int
maps p i = fromMaybe i (IM.lookup i $ pIMap p)

-- | Converts pairs of initial and final locations to a permutation.
fromPairs :: [(Int,Int)] -- ^ A list of pairs; the first element is the
                         -- source location, the second the destination.
          -> Permutation
fromPairs = Permutation . IM.fromList

-- | Inverse of 'fromPairs'.
toPairs :: Permutation -> [(Int,Int)]
toPairs = IM.toList . pIMap

-- | Defined by @ 'permute' ('invert' p) . 'permute' p $ l = l @.
invert :: Permutation -> Permutation
invert (Permutation im) = Permutation . IM.fromList . map swap . IM.assocs $ im

-- | Defined by @'permute' (p2 `compose` p1) = 'permute' p2 . 'permute' p1@.
compose :: Permutation -> Permutation -> Permutation
(Permutation p2) `compose` (Permutation p1) = Permutation $ IM.union p1' p2
  where p1' = IM.map comp p1
        comp a = fromMaybe a $ IM.lookup a p2

-- | General initial swap permutation.
swap01 :: Permutation
swap01 = Permutation $ IM.fromDistinctAscList [(0,1),(1,0)]

-- | Special case of 'sortByListWithP' using the 'Ord' instance compare.
sortListWithP :: (Ord a) => [a] -> ([a], Permutation)
sortListWithP = sortByListWithP compare

-- | Get the sorted list along with the permutation that would have done the sorting.
sortByListWithP :: (a -> a -> Ordering) -> [a] -> ([a], Permutation)
sortByListWithP sf l = let (sl, invpl) = unzip . sortBy (\(a,_) (b,_) -> sf a b) . flip zip [0..] $ l
                       in (sl, invert . unsafeFromList $ invpl)

-- | Class for objects that can be permuted.
class Permutable s where
  -- | Function that enacts a permutation on an object, returning a permuted version.
  permute :: Permutation -> s -> s

instance Permutable Int where
  permute _ = id

instance Permutable (a,a) where
  permute p = if IM.null $ pIMap p
                 then id
                 else swap

cmpNegInx :: Int -> Int -> Ordering
cmpNegInx a b | (a >= 0) && (b >= 0) = compare a b
              | (a >= 0) && (b < 0) = LT
              | (a < 0) && (b >= 0) = GT
              | otherwise = compare a b

instance Permutable [a] where
  permute p = fmap snd . sortBy (cmpNegInx `on` fst) . zip (toList p)

instance Permutable (Seq a) where
  permute p = fmap snd . SQ.sortBy (cmpNegInx `on` fst) . SQ.mapWithIndex ((,) . maps p)
