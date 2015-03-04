{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tensor.Slice
       (InxSlice(..)
       ,slModShape
       ,slModScheme
       ,fromSlInx
       
       ,Slicer
       ,SliceTo(..))
where

import Data.Maybe (fromJust)

-- | Describes a subset of indices in some indexed type.
data InxSlice a
  = Inx !a -- ^ take a single member
  | Inxs [a] -- ^ take a list of members (by ordinal)
  | InxRangeStep !(a,a,a)  -- ^ like 'InxRange' with an 'InxStep' step (step
                           -- will never cross over upper bound)
  | InxRange !(a,a) -- ^ take all members whose index is in the half-open interval @[fst,snd)@
  | InxStep !a -- ^ take every 'InxStep' element
  | All -- ^ take all members
  deriving (Eq,Ord,Show)

-- | Information regarding new shape parameters.
slModShape :: InxSlice Int 
           -> Int -- ^ Previous shape value
           -> Int -- ^ New shape value.
slModShape (Inx _) _ = 1
slModShape (Inxs as) _ = length as
slModShape (InxRangeStep (l,u,s)) _  = (u - l) `quot` s
slModShape (InxRange (l,u)) _ = u - l
slModShape (InxStep st) s = s `quot` st
slModShape All s = s

-- | Information regarding modification of standard dense numerical
-- scheme with slicer.
slModScheme :: InxSlice Int 
            -> Int 
            -> Maybe (Int, Int) -- ^ Pair of (new offset, new step)
slModScheme (Inx i) cstep = Just (cstep * i, i)
slModScheme (Inxs _) _ = Nothing
slModScheme (InxRangeStep (l,_,step)) cstep = Just (l * cstep, step * cstep)
slModScheme (InxRange (l, _)) cstep = Just (l * cstep, cstep)
slModScheme (InxStep nstep) cstep = Just (0, nstep * cstep)
slModScheme All cstep = Just (0, cstep)

fromSlInx :: InxSlice Int -> Int -> Int
fromSlInx (Inx i) _ = i
fromSlInx (Inxs is) j = is !! j
fromSlInx (InxRangeStep (l,_,s)) j = l + s * j
fromSlInx (InxRange (l,_)) j = l + j
fromSlInx (InxStep s) j = s * j
fromSlInx All j = j

type family Slicer t :: *

class SliceTo ta tb where
  (.?) :: ta -> Slicer ta -> tb
  (.?) t = fromJust . slice t

  slice :: ta -> Slicer ta -> Maybe tb
  slice t = Just . (.?) t
