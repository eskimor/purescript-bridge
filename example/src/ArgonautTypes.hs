{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeApplications  #-}

module ArgonautTypes where

import           Language.PureScript.Bridge (Language (Haskell),
                                             argonautAesonGeneric, mkSumType)
import           Language.PureScript.Bridge.SumType (SumType)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Types

myArgonautTypes :: [SumType 'Haskell]
myArgonautTypes =
  [ argonautAesonGeneric . additionalInstances $ mkSumType @Baz
  , argonautAesonGeneric . additionalInstances $ mkSumType @Foo
  , argonautAesonGeneric . additionalInstances $ mkSumType @(Bar A)
  , argonautAesonGeneric . additionalInstances $ mkSumType @TestSum
  , argonautAesonGeneric . additionalInstances $ mkSumType @TestData
  ]
