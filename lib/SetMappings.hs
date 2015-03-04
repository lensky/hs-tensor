{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SetMappings
       (Subset(..))
where

import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.Foldable (toList)

-- | Class for types 'a' that are subsets of type 'b'.
class Subset a b where
  -- | Converts from the subset to the superset.
  toSuperSet :: a -> b

  -- | Converts from the superset to the subset, possibly failing.
  fromSuperSet :: b -> Maybe a
  fromSuperSet = Just . unsafeFromSuperSet

  -- | Unsafe version of 'fromSuperSet', default implementation in terms of 'fromSuperSet' provided.
  unsafeFromSuperSet :: b -> a
  unsafeFromSuperSet = fromJust . fromSuperSet

instance Subset a a where
  toSuperSet = id
  fromSuperSet = Just

instance Subset (a,a) [a] where
  toSuperSet (i,j) = [i,j]
  fromSuperSet [i,j] = Just (i,j)
  fromSuperSet _ = Nothing

instance Subset a [a] where
  toSuperSet = flip (:) []
  fromSuperSet [a] = Just a
  fromSuperSet _ = Nothing

instance Subset Bool Int where
  toSuperSet True = 1
  toSuperSet False = 0
  
  fromSuperSet 1 = Just True
  fromSuperSet 0 = Just False
  fromSuperSet _ = Nothing

instance Subset (Seq a) [a] where
  toSuperSet = toList
  unsafeFromSuperSet = SQ.fromList

instance Subset a (Seq a) where
  toSuperSet = SQ.singleton
  fromSuperSet s = case SQ.viewl s of
                     SQ.EmptyL -> Nothing
                     h SQ.:< r -> if SQ.null r
                                     then Just h
                                     else Nothing

instance Subset [a] (Seq a) where
  toSuperSet = SQ.fromList
  unsafeFromSuperSet = toList
