{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeApplications  #-}

module JsonHelpersTypes where

import           Language.PureScript.Bridge (Language (Haskell), jsonHelpers,
                                             mkSumType)
import           Language.PureScript.Bridge.SumType (SumType)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Types

myJsonHelpersTypes :: [SumType 'Haskell]
myJsonHelpersTypes =
  [ jsonHelpers . additionalInstances $ mkSumType @Baz
  , jsonHelpers . additionalInstances $ mkSumType @Foo
  , jsonHelpers . additionalInstances $ mkSumType @(Bar A)
  , jsonHelpers . additionalInstances $ mkSumType @TestSum
  , jsonHelpers . additionalInstances $ mkSumType @TestData
  ]
