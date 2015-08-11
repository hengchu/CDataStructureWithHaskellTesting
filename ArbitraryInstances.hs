{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module ArbitraryInstances
()
where

import Test.QuickCheck
import Foreign.C.Types

instance Arbitrary CIntPtr where
  arbitrary = do
    int <- arbitrary :: Gen Word
    return $ fromIntegral int

instance CoArbitrary CIntPtr where
  coarbitrary = coarbitraryIntegral

instance Show (CIntPtr -> Bool) where
  show _ = "func: CIntPtr -> Bool"
