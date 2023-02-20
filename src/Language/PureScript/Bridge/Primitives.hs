{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module Language.PureScript.Bridge.Primitives where


import           Control.Monad.Reader.Class
import           Data.Proxy
import           Language.PureScript.Bridge.Builder
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeInfo


boolBridge :: BridgePart
boolBridge = typeName ^== "Bool" >> return psBool

eitherBridge :: BridgePart
eitherBridge = typeName ^== "Either" >> psEither

strMapBridge :: BridgePart
strMapBridge = typeName ^== "Map" >> psObject

-- | Dummy bridge, translates every type with 'clearPackageFixUp'
dummyBridge :: MonadReader BridgeData m => m PSType
dummyBridge = clearPackageFixUp

intBridge :: BridgePart
intBridge = typeName ^== "Int" >> return psInt

doubleBridge :: BridgePart
doubleBridge = typeName ^== "Double" >> return psNumber

listBridge :: BridgePart
listBridge = typeName ^== "[]" >> psArray

maybeBridge :: BridgePart
maybeBridge = typeName ^== "Maybe" >> psMaybe

stringBridge :: BridgePart
stringBridge = haskType ^== mkTypeInfo (Proxy :: Proxy String ) >> return psString

textBridge :: BridgePart
textBridge = do
    typeName   ^== "Text"
    typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
    return psString

unitBridge :: BridgePart
unitBridge = typeName ^== "()" >> return psUnit

noContentBridge :: BridgePart
noContentBridge = typeName ^== "NoContent" >> return psUnit

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> return psWord

word8Bridge :: BridgePart
word8Bridge = typeName ^== "Word8" >> return psWord8

word16Bridge :: BridgePart
word16Bridge = typeName ^== "Word16" >> return psWord16

word32Bridge :: BridgePart
word32Bridge = typeName ^== "Word32" >> return psWord32

word64Bridge :: BridgePart
word64Bridge = typeName ^== "Word64" >> return psWord64
