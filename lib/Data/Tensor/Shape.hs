{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Tensor.Shape
       (Shape(..)
       ,ShapeGD
       ,Shape2D)
where

import Prelude hiding (foldr)

import SetMappings
import Data.Permutation (Permutable)
import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import Data.Foldable (foldr)

-- | Class for objects that can specify the shape of a tensor. A canonical
-- example is @[Int]@, but using for example tuples allows us to restrict the
-- dimensionality of a tensor.
class (Permutable ts, Subset ts [Int], Eq ts) => Shape ts where
  -- | Every tensor shape must have some canonical representation as a list
  -- of dimensions. In the usual @[Int]@ case this is a direction
  -- transliteration.
  toList :: ts -> [Int]
  toList = toSuperSet 

  -- | List as shape defined as the right-inverse of 'toList' : @
  -- (toList . fromList) ts = ts @.
  fromList :: [Int] -> Maybe ts
  fromList = fromSuperSet

  -- | Unsafe version of 'fromList'.
  unsafeFromList :: [Int] -> ts
  unsafeFromList = unsafeFromSuperSet

  -- | The "dimension" of the shape.
  rank :: ts -> Int
  rank = length . toList

  -- | The size of the tensor with this shape.
  size :: ts -> Int
  size = product . toList
  
-- | Naive list-based tensor shape. Generally speaking, each element is one
-- more than the maximal index allowed in the tensor.
instance Shape [Int] where

-- | Matrix-like tensor shape, explicitly restricted to two dimensions. Can be
-- faster than [Int] when restricted to a rank-2 object.
instance Shape (Int,Int) where
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
  
  rank _ = 2
  size (a,b) = a*b

-- | Vector-like tensor shape.
instance Shape Int where
  rank _ = 1
  size = id

instance Shape (Seq Int) where
  rank = SQ.length
  size s = if SQ.null s
              then 0
              else foldr (*) 1 s

type ShapeGD = Seq Int
type Shape2D = (Int,Int)
