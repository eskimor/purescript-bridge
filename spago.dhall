{ name = "purescript-bridge-example"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut"
  , "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign-generic"
  , "foreign-object"
  , "json-helpers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  ]
, packages = ./example/packages.dhall
, sources =
  [ "example/src/**/*.purs"
  , "example/test/**/*.purs"
  , "test/RoundTripArgonautAesonGeneric/app/src/RoundTripArgonautAesonGeneric/*.purs"
  , "test/RoundTripJsonHelpers/app/src/RoundTripJsonHelpers/*.purs"
  ]
}
