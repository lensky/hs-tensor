{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tensor.Scheme.Dense2D
       (Dense2D
       ,mkRowMajorScheme
       ,mkColMajorScheme)
where

import Data.Tuple (swap)
import Data.Monoid (mempty)
import qualified Data.Vector.Generic as GV

import Data.Tensor.Shape (rank, Shape2D)
import Data.Tensor.Scheme (Scheme(..), VectorScheme(..), DenseScheme(..))

data Dense2D = Dense2D { _colMajor :: Bool
                       , _s2dShape :: Shape2D }

mkRowMajorScheme :: Shape2D -> Dense2D
mkRowMajorScheme s = Dense2D { _colMajor = False, _s2dShape = s }

mkColMajorScheme :: Shape2D -> Dense2D
mkColMajorScheme s = Dense2D { _colMajor = True, _s2dShape = s }

instance Scheme Dense2D where
  type SchemeSelector Dense2D = (Int,Int)
  type SchemeShape Dense2D = Shape2D

  selectorToInx sc s = let i = unsafeSelectorToInx sc s
                       in if (0 <= i) && (i < storageSize sc)
                            then Just i
                            else Nothing

  unsafeSelectorToInx (Dense2D cm (rs,cs)) (r,c) = 
    if cm
       then c * rs + r
       else r * cs + c

  inxToSelector (Dense2D cm (rs,cs)) i = if valid
                                           then Just (r,c)
                                           else Nothing
    where valid = (r >= 0) && (c >= 0) && (r < rs) && (c < cs)
          (r,c) = if cm
                     then swap $ i `quotRem` rs
                     else i `quotRem` cs

  shape (Dense2D _ s) = s

  storageSize = rank . shape

  permuteInxs p sc@(Dense2D cm s) = if p == mempty
                                      then sc
                                      else Dense2D (not cm) (swap s)

instance (GV.Vector v e) => VectorScheme Dense2D v e where
  getElt = getElt . DenseScheme
  unsafeGetElt = unsafeGetElt . DenseScheme
