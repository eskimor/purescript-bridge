let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

let overrides =
      { argonaut-generic = upstream.argonaut-generic ⫽ { version = "v5.0.0" }
      , argonaut = upstream.argonaut ⫽ { version = "v6.0.0" }
      , argonaut-codecs = upstream.argonaut-codecs ⫽ { version = "v6.0.2" }
      , argonaut-traversals =
          upstream.argonaut-traversals ⫽ { version = "v6.0.0" }
      }

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
        , repo = "git://github.com/coot/purescript-argonaut-aeson-generic.git"
        , version = "2201093f39d58befe7e4ae9e2f587e340ee54a28"
        }
      }

in  upstream ⫽ overrides ⫽ additions
