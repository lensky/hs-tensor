{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Tensor.Dense
       (SliceTo(..)
       
       ,GTensorGD
       ,GMatrix
       )
where

import Prelude

import Data.Foldable (foldlM)

import Data.Sequence (Seq)
import qualified Data.Sequence as SQ

import qualified Data.Vector.Generic as GV

import Data.Tensor
import Data.Tensor.Scheme (shape)
import Data.Tensor.Scheme.Dense2D
import Data.Tensor.Scheme.DenseGD
import Data.Tensor.Slice

type GTensorGD = Tensor DenseGD
type GMatrix = Tensor Dense2D

type instance Slicer (GTensorGD v e) = Seq (InxSlice Int)
instance (GV.Vector v e) => SliceTo (GTensorGD v e) (GTensorGD v e) where
  t .? sls' = case schemeMod of
                Just (offset, mtable) -> t { _scheme = oscheme { _sgdShape = shape'
                                                               , _sgdMTable = mtable
                                                               , _sgdOffset = offset } }
                Nothing -> generate (mkDefaultDenseGD shape') 
                                    ((.!) t . SQ.zipWith fromSlInx sls')
    where oscheme = _scheme t
          shape' = SQ.zipWith slModShape sls' . shape . _scheme $ t
          schemeMod = foldlM (fmap . f) (0, SQ.empty) 
                             (SQ.zipWith slModScheme sls' (_sgdMTable . _scheme $ t))
            where f (off, ms) (o, m) = (off + o, ms SQ.|> m)
