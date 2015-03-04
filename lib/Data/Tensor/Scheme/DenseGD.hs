{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tensor.Scheme.DenseGD
       (DenseGD(..)
       ,mkDenseGD
       ,mkDefaultDenseGD)
where

import Prelude hiding (foldr1)

import Data.Foldable (foldl', foldr1)
import Data.Monoid (mempty, (<>))

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ
import qualified Data.Vector.Generic as GV

import Data.Permutation (Permutation, permute, invert)

import Data.Tensor.Shape (ShapeGD,rank)
import Data.Tensor.Scheme (Scheme(..),VectorScheme(..),DenseScheme(..))

data DenseGD = DenseGD { _sgdShape :: ShapeGD
                       , _sgdStorageOrder :: Permutation
                       , _sgdMTable :: Seq Int
                       , _sgdOffset :: Int }

mkDenseGD :: Permutation -> Int -> ShapeGD -> DenseGD
mkDenseGD so off s = DenseGD s so mtable off
  where mtable = permute (invert so) . SQ.scanr (*) 1 . SQ.drop 1 . permute so $ s

mkDefaultDenseGD :: ShapeGD -> DenseGD
mkDefaultDenseGD = mkDenseGD mempty 0

sgdRank :: DenseGD -> Int
sgdRank = rank . _sgdShape

instance Scheme DenseGD where
  type SchemeSelector DenseGD = Seq Int
  type SchemeShape DenseGD = ShapeGD

  unsafeSelectorToInx sc = foldr1 (+) . SQ.zipWith (*) (_sgdMTable sc)

  selectorToInx sc sel = if diffLength || inxOutOfRange
                            then Nothing
                            else Just us
    where diffLength = SQ.length (_sgdMTable sc) /= SQ.length sel
          inxOutOfRange = (us < 0) || (us >= rnk)
          us = unsafeSelectorToInx sc sel
          rnk = sgdRank sc

  inxToSelector sc i = if (i < 0) || (i >= sgdRank sc)
                          then Nothing
                          else Just . permute (invert $ _sgdStorageOrder sc) $ soSel
    where soSel = fst . foldl' selStep (SQ.empty, i) $ soMTable
          selStep (tsel,i') c = let (s,i'') = i' `quotRem` c
                                in (tsel SQ.|> s, i'')
          soMTable = permute (_sgdStorageOrder sc) (_sgdMTable sc)

  shape = _sgdShape

  storageSize = rank . shape

  permuteInxs p sc = mkDenseGD (p <> _sgdStorageOrder sc) 0 (_sgdShape sc)

instance (GV.Vector v e) => VectorScheme DenseGD v e where
  getElt = getElt . DenseScheme
  unsafeGetElt = unsafeGetElt . DenseScheme
