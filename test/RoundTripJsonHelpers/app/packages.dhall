let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230331/packages.dhall
        sha256:97a54e4c5c1a76f51cef8fb8c91a8ff602dca7828dc464e07e48ee563b6bd058

let additions =
      { argonaut-aeson-generic =
        { dependencies =
          [ "argonaut"
          , "argonaut-codecs"
          , "argonaut-generic"
          , "console"
          , "effect"
          , "foreign-object"
          , "test-unit"
          ]
        , repo =
            "https://github.com/peterbecich/purescript-argonaut-aeson-generic.git"
        , version = "e22b1b9046aef15d6441ea90870dfbfa455a70fb"
        }
      , foreign-generic =
        { dependencies =
          [ "effect"
          , "foreign"
          , "foreign-object"
          , "ordered-collections"
          , "exceptions"
          , "record"
          , "identity"
          ]
        , repo = "https://github.com/jsparkes/purescript-foreign-generic.git"
        , version = "844f2ababa2c7a0482bf871e1e6bf970b7e51313"
        }
      , json-helpers =
          { dependencies =
            [ "aff"
            , "argonaut-codecs"
            , "argonaut-core"
            , "arrays"
            , "bifunctors"
            , "contravariant"
            , "control"
            , "effect"
            , "either"
            , "enums"
            , "foldable-traversable"
            , "foreign-object"
            , "maybe"
            , "newtype"
            , "ordered-collections"
            , "prelude"
            , "profunctor"
            , "psci-support"
            , "quickcheck"
            , "record"
            , "spec"
            , "spec-quickcheck"
            , "transformers"
            , "tuples"
            , "typelevel-prelude"
            ]
          , repo =
              "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
          , version = "60615c36abaee16d8dbe09cdd0e772e6d523d024"
          }
      }


in  upstream // additions // {
  json-helpers =
    { dependencies =
      [ "aff"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "contravariant"
      , "control"
      , "effect"
      , "either"
      , "enums"
      , "foldable-traversable"
      , "foreign-object"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "profunctor"
      , "psci-support"
      , "quickcheck"
      , "record"
      , "spec"
      , "spec-quickcheck"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      ]
    , repo = "https://github.com/input-output-hk/purescript-bridge-json-helpers.git"
    , version = "60615c36abaee16d8dbe09cdd0e772e6d523d024"
    }
}