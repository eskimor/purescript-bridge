{ name = "purescript-bridge-example"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-generic"
  , "profunctor-lenses"
  , "aff"
  , "affjax"
  , "affjax-web"
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
, packages = ./example/packages.dhall
, sources = [ "example/src/**/*.purs", "example/test/**/*.purs" ]
}
