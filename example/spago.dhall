{-
Welcome to a Spago project!
You can edit this file as you like.
-}
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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
