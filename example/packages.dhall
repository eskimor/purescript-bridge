let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210304/packages.dhall sha256:c88151fe7c05f05290224c9c1ae4a22905060424fb01071b691d3fe2e5bad4ca

let additions =
      { argonaut-aeson-generic =
        { dependencies =
          [ "argonaut"
          , "argonaut-codecs"
          , "argonaut-generic"
          , "console"
          , "effect"
          , "foreign-object"
          , "psci-support"
          , "test-unit"
          ]
        , repo =
            "git://github.com/peterbecich/purescript-argonaut-aeson-generic.git"
        , version = "2c8c5ee2381ddb786af7fb79a73e3b83001d68e8"
        }
      , foreign-generic =
        { dependencies =
          [ "console"
          , "effect"
          , "psci-support"
          , "prelude"
          , "tuples"
          , "bifunctors"
          , "foreign"
          , "foreign-object"
          , "assert"
          , "record"
          ]
        , repo = "git://github.com/peterbecich/purescript-foreign-generic.git"
        , version = "56bc2056ef706ded4ef4406aced01a23d39af7cf"
        }
      }

in  upstream // additions
