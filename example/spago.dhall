{ name = "purescript-bridge-example"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "foreign-generic"
  , "profunctor-lenses"
  , "aff"
  , "affjax"
  , "argonaut-aeson-generic"
  , "ordered-collections"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "maybe"
  , "newtype"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
