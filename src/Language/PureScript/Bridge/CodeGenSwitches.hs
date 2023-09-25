-- | General switches for the code generation, such as generating profunctor-lenses or not
module Language.PureScript.Bridge.CodeGenSwitches
    ( Settings (..)
    , ForeignOptions (..)
    , defaultSettings
    , Switch
    , getSettings
    , defaultSwitch
    , noLenses
    , genLenses
    , noArgonautCodecs
    , genArgonautCodecs
    ) where

import           Data.Monoid (Endo (..))

-- | General settings for code generation
data Settings = Settings
  { generateLenses         :: Bool
    -- ^ use purescript-profunctor-lens for generated PS-types?
  , generateArgonautCodecs :: Bool
    -- ^ generate Data.Argonaut.Decode.Class EncodeJson and DecodeJson instances
  }
  deriving (Eq, Show)

data ForeignOptions = ForeignOptions
  { unwrapSingleConstructors :: Bool
  , unwrapSingleArguments    :: Bool
  }
  deriving (Eq, Show)

-- | Settings to generate Lenses
defaultSettings :: Settings
defaultSettings = Settings True True

-- | you can `mappend` switches to control the code generation
type Switch = Endo Settings

-- | Translate switches into settings
getSettings :: Switch -> Settings
getSettings switch = appEndo switch defaultSettings

-- | Default switches include code generation for lenses
defaultSwitch :: Switch
defaultSwitch = mempty

-- | Switch off the generation of profunctor-lenses
noLenses :: Switch
noLenses = Endo $ \settings -> settings {generateLenses = False}

-- | Switch off the generation of argonaut-codecs
noArgonautCodecs :: Switch
noArgonautCodecs = Endo $ \settings ->
    settings {generateArgonautCodecs = False}

-- | Switch on the generation of profunctor-lenses
genLenses :: Switch
genLenses = Endo $ \settings -> settings {generateLenses = True}

genArgonautCodecs :: Switch
genArgonautCodecs = Endo $ \settings ->
    settings {generateArgonautCodecs = True}

