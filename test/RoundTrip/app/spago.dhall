{ name = "my-project"
, dependencies =
  [ "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "foreign-object"
  , "maybe"
  , "newtype"
  , "node-readline"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "tuples"
  , "json-helpers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
