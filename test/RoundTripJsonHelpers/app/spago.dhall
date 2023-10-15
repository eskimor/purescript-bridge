{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "foreign-object"
  , "json-helpers"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
