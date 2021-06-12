let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210516/packages.dhall sha256:f5e978371d4cdc4b916add9011021509c8d869f4c3f6d0d2694c0e03a85046c8

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
        , repo = "git://github.com/paf31/purescript-foreign-generic.git"
        , version = "3cddc5fe3e87e426e0f719465ba60b9df4c0c72d"
        }
      }

in  upstream // additions
