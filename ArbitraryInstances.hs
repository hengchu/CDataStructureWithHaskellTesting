{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances
()
where

import Test.QuickCheck
import Foreign.C.Types

instance Arbitrary CIntPtr where
  arbitrary = do
    int <- arbitrary :: Gen Word
    return $ fromIntegral int
