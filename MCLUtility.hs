module MCLUtility
(
  MCLComparator
, MCLPred
)
where

import Foreign.C.Types

type MCLComparator = CIntPtr -> CIntPtr 
                             -> CIntPtr 
                             -> CUChar

type MCLPred = CIntPtr -> CIntPtr -> CUChar
