-- | General switches for the code generation, such as generating profunctor-lenses or not
module Language.PureScript.Bridge.CodeGenSwitches
    ( ForeignOptions (..)
    , Settings (..)
    , Switch
    , defaultSettings
    , defaultSwitch
    , genArgonautCodecs
    , genForeign
    , genLenses
    , getSettings
    , noArgonautCodecs
    , noForeign
    , noLenses
    , useGen
    , useGenRep
    ) where

import           Data.Monoid (Endo (..))

-- | General settings for code generation
data Settings = Settings
  { generateLenses         :: Bool
    -- ^ use purescript-profunctor-lens for generated PS-types?
  , genericsGenRep         :: Bool
    -- ^ generate generics using purescript-generics-rep instead of purescript-generics
  , generateArgonautCodecs :: Bool
    -- ^ generate Data.Argonaut.Decode.Class EncodeJson and DecodeJson instances
  , generateForeign        :: Maybe ForeignOptions
    -- ^ generate Foreign.Generic Encode and Decode instances
  }
  deriving (Eq, Show)

data ForeignOptions = ForeignOptions
  { unwrapSingleConstructors :: Bool
  , unwrapSingleArguments    :: Bool
  }
  deriving (Eq, Show)

-- | Settings to generate Lenses
defaultSettings :: Settings
defaultSettings = Settings True True True Nothing

-- | you can `mappend` switches to control the code generation
type Switch = Endo Settings

-- | Translate switches into settings
getSettings :: Switch -> Settings
getSettings switch = appEndo switch defaultSettings

-- | Default switches include code generation for lenses
defaultSwitch :: Switch
defaultSwitch = mempty

-- | Switch off the generatation of profunctor-lenses
noLenses :: Switch
noLenses = Endo $ \settings -> settings {generateLenses = False}

-- | Switch off the generatation of argonaut-codecs
noArgonautCodecs :: Switch
noArgonautCodecs = Endo $ \settings ->
    settings {generateArgonautCodecs = False}

-- | Switch on the generatation of profunctor-lenses
genLenses :: Switch
genLenses = Endo $ \settings -> settings {generateLenses = True}

-- | Generate generics using purescript-generics-rep
useGenRep :: Switch
useGenRep = Endo $ \settings -> settings {genericsGenRep = True}

-- | Generate generics using purescript-generics
useGen :: Switch
useGen = Endo $ \settings -> settings {genericsGenRep = False}

genForeign :: ForeignOptions -> Switch
genForeign opts = Endo $ \settings -> settings {generateForeign = Just opts}

genArgonautCodecs :: Switch
genArgonautCodecs = Endo $ \settings ->
    settings {generateArgonautCodecs = True}

noForeign :: Switch
noForeign = Endo $ \settings -> settings {generateForeign = Nothing}
