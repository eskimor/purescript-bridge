let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220901/packages.dhall
        sha256:f1531b29c21ac437ffe5666c1b6cc76f0a9c29d3c9d107ff047aa2567744994f

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
            "https://github.com/coot/purescript-argonaut-aeson-generic.git"
        , version = "v0.4.1"
        }
      , argonaut-codecs =
        { dependencies = [ "console" ]
        , repo = "https://github.com/peterbecich/purescript-argonaut-codecs.git"
        , version = "04abb3eb24a4deafe125be0eb23e2786c642e66b"
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

in  upstream // additions
