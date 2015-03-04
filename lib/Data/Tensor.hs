{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Tensor
       (
       -- * Primary datatype
       Tensor(..)

       -- ** Lenses
       ,scheme
       ,rawData
       
       -- * Element extraction
       ,(.!)
       ,(.!!)
       ,getElt
       ,getEltByList

       -- * Construction
       ,generate
       ,generateM

       -- * Index operations
       ,reIndex)
where

import Control.Lens (makeLenses)

import qualified Data.Vector.Generic as GV

import SetMappings
import Data.Permutation (Permutation)

import Data.Tensor.Scheme hiding (getElt, getEltByList)
import qualified Data.Tensor.Scheme as SC

data Tensor sc v e = Tensor { _scheme :: sc
                            , _rawData :: v e }
makeLenses ''Tensor

instance (Functor v) => Functor (Tensor sc v) where
  fmap f (Tensor sc rd) = Tensor sc (fmap f rd)

(.!) :: (VectorScheme sc v e) => Tensor sc v e -> SchemeSelector sc -> e
t .! s = unsafeGetElt (_scheme t) (_rawData t) s

(.!!) :: (VectorScheme sc v e, Subset (SchemeSelector sc) [Int]) 
      => Tensor sc v e -> [Int] -> e
t .!! s = unsafeGetEltByList (_scheme t) (_rawData t) s

getElt :: (VectorScheme sc v e) => Tensor sc v e -> SchemeSelector sc -> Maybe e
getElt t = SC.getElt (_scheme t) (_rawData t)

getEltByList :: (VectorScheme sc v e, Subset (SchemeSelector sc) [Int]) 
             => Tensor sc v e -> [Int] -> Maybe e
getEltByList t = SC.getEltByList (_scheme t) (_rawData t)

reIndex :: (Scheme sc) => Permutation -> Tensor sc v e -> Tensor sc v e
reIndex p t = t { _scheme = permuteInxs p $ _scheme t }

generate :: (Scheme sc, GV.Vector v e) 
         => sc -> (SchemeSelector sc -> e) -> Tensor sc v e
generate sc f = Tensor sc (generateVector sc f)

generateM :: (Scheme sc, GV.Vector v e, Monad m, Functor m)
          => sc -> (SchemeSelector sc -> m e) -> m (Tensor sc v e)
generateM sc = fmap (Tensor sc) . generateMVector sc
