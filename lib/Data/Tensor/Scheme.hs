{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tensor.Scheme
       (Scheme(..)
       ,VectorScheme(..)
       ,DenseScheme(..)
       ,generateVector
       ,generateMVector
       ,listToInx
       ,unsafeListToInx
       ,getEltByList
       ,unsafeGetEltByList)
where

import Control.Monad ((>=>))
import Data.Maybe (fromJust)

import Data.Vector.Generic as GV

import SetMappings

import Data.Permutation (Permutation)
import Data.Tensor.Shape (Shape)

class Shape (SchemeShape sc) => Scheme sc where
  type SchemeSelector sc :: *
  type SchemeShape sc :: *

  selectorToInx :: sc -> SchemeSelector sc -> Maybe Int

  unsafeSelectorToInx :: sc -> SchemeSelector sc -> Int
  unsafeSelectorToInx sc = fromJust . selectorToInx sc

  inxToSelector :: sc -> Int -> Maybe (SchemeSelector sc)

  shape :: sc -> SchemeShape sc
  permuteInxs :: Permutation -> sc -> sc
  
  storageSize :: sc -> Int

class (Scheme sc, GV.Vector v e) => VectorScheme sc v e where
  getElt :: sc -> v e -> SchemeSelector sc -> Maybe e
  unsafeGetElt :: sc -> v e -> SchemeSelector sc -> e
  unsafeGetElt sc v = fromJust . getElt sc v

newtype DenseScheme sc = DenseScheme { unDenseScheme :: sc }

instance (Scheme sc) => Scheme (DenseScheme sc) where
  type SchemeSelector (DenseScheme sc) = SchemeSelector sc
  type SchemeShape (DenseScheme sc) = SchemeShape sc

  selectorToInx = selectorToInx . unDenseScheme
  unsafeSelectorToInx = unsafeSelectorToInx . unDenseScheme

  inxToSelector = inxToSelector . unDenseScheme
  shape = shape . unDenseScheme
  permuteInxs p = DenseScheme . permuteInxs p . unDenseScheme
  storageSize = storageSize . unDenseScheme

instance (Scheme sc, GV.Vector v e) => VectorScheme (DenseScheme sc) v e where
  getElt (DenseScheme sc) v sel = fmap (v GV.!) (selectorToInx sc sel)
  unsafeGetElt (DenseScheme sc) v sel = v GV.! unsafeSelectorToInx sc sel

selFToInxF :: (Scheme sc) => sc -> (SchemeSelector sc -> e) -> Int -> e
selFToInxF sc f = f . fromJust . inxToSelector sc

generateVector :: (Scheme sc, GV.Vector v e) 
               => sc -> (SchemeSelector sc -> e) -> v e
generateVector sc = GV.generate (storageSize sc) . selFToInxF sc

generateMVector :: (Scheme sc, GV.Vector v e, Monad m)
                => sc -> (SchemeSelector sc -> m e) -> m (v e)
generateMVector sc = GV.generateM (storageSize sc) . selFToInxF sc

listToInx :: (Scheme sc, Subset (SchemeSelector sc) [Int]) => sc -> [Int] -> Maybe Int
listToInx sc = fromSuperSet >=> selectorToInx sc

unsafeListToInx :: (Scheme sc, Subset (SchemeSelector sc) [Int]) => sc -> [Int] -> Int
unsafeListToInx sc = unsafeSelectorToInx sc . unsafeFromSuperSet

getEltByList :: (VectorScheme sc v e, Subset (SchemeSelector sc) [Int]) 
             => sc -> v e -> [Int] -> Maybe e
getEltByList sc v sel = fromSuperSet sel >>= getElt sc v

unsafeGetEltByList :: (VectorScheme sc v e, Subset (SchemeSelector sc) [Int]) 
                   => sc -> v e -> [Int] -> e
unsafeGetEltByList sc v sel = unsafeGetElt sc v (unsafeFromSuperSet sel)
