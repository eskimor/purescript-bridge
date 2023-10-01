{ name = "my-project"
, dependencies =
  [ "foreign-object"
  , "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "argonaut-core"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
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
