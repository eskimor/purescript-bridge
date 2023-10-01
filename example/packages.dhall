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
      }

in  upstream // additions
