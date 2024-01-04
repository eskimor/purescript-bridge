# purescript-bridge
[![Haskell library and example](https://github.com/eskimor/purescript-bridge/actions/workflows/haskell.yml/badge.svg)](https://github.com/eskimor/purescript-bridge/actions/workflows/haskell.yml) [![PureScript example](https://github.com/eskimor/purescript-bridge/actions/workflows/purescript.yml/badge.svg)](https://github.com/eskimor/purescript-bridge/actions/workflows/purescript.yml) [![Nix Flake](https://github.com/eskimor/purescript-bridge/actions/workflows/nix-flake.yml/badge.svg)](https://github.com/eskimor/purescript-bridge/actions/workflows/nix-flake.yml)

Translate your Haskell types to PureScript types. It should in theory work for almost all Haskell types, including type constructors!
You just have to instantiate it with dummy parameters from e.g. "Language.PureScript.Bridge.TypeParameters".

Data type translation is fully and easily customizable by providing your own `BridgePart` instances!

The latest version of this project requires **PureScript 0.15**.

## JSON encoding / decoding
### Haskell
Use [`aeson`](http://hackage.haskell.org/package/aeson)'s generic encoding/decoding with default options

### PureScript
There are three PureScript libraries which can interface with Aeson through PureScript bridge. The second, `purescript-argonaut-aeson-generic`, has issues.

#### [`input-output-hk/purescript-bridge-json-helpers`](https://github.com/input-output-hk/purescript-bridge-json-helpers.git)

Enable this on the Haskell side with `Language.PureScript.Bridge.SumType.jsonHelpers`.

* see `./test/RoundTripJsonHelpers` for example
* sample Dhall config (for [spago-legacy](https://github.com/purescript/spago-legacy)):
```
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
  , version = "486db9ee62882baa42cca24f556848c5f6bec565"
  }
```

#### [`purescript-argonaut-aeson-generic >=0.4.1`](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic/0.4.1) ([GitHub](https://github.com/coot/purescript-argonaut-aeson-generic))

Enable this on the Haskell side with `Language.PureScript.Bridge.SumType.argonautAesonGeneric`.

This library is demonstrated by the `example`; see `./example/readme.md`.

**TODO**: [resolve incompatibility between Argonaut and Aeson](https://github.com/purescript-contrib/purescript-argonaut-codecs/issues/115)

**Additional requirement**: [`peterbecich/purescript-argonaut-codecs`](https://github.com/peterbecich/purescript-argonaut-codecs.git)
* commit `04abb3eb24a4deafe125be0eb23e2786c642e66b`
* see `./test/RoundTripArgonautAesonGeneric` for example
  * note that some types have been disabled from the `RoundTripArgonautAesonGeneric` test
    * `RoundTripJsonHelpers` tests more types
  * the types tested can be expanded when the incompatibility issue is resolved
* sample Dhall config:
```
      , argonaut-codecs =
        { dependencies = [ "console" ]
        , repo = "https://github.com/peterbecich/purescript-argonaut-codecs.git"
        , version = "04abb3eb24a4deafe125be0eb23e2786c642e66b"
        }
```
* forked from [`purescript-contrib/purescript-argonaut-codecs`](https://github.com/purescript-contrib/purescript-argonaut-codecs)

#### [`paf31/purescript-foreign-generic`](https://github.com/paf31/purescript-foreign-generic)

See `ForeignObject` in `Language.PureScript.Bridge.SumType`.

This may need to be fixed.

The test coverage is less than the other two libraries.

## Documentation

Usage of this library is documented in `Language.Purescript.Bridge`, with `writePSTypes` you should have everything to get started. Documentation can be found [here](https://www.stackage.org/nightly/package/purescript-bridge).

There is an example; see `./example/readme.md`.

## Status

It works for my use case and is used in production. PRs for more `PSType`s definitions and bridges are very welcome! 

## Contributing

### Formatting the source code

This repo uses [`stylish-haskell`](https://github.com/haskell/stylish-haskell). `.stylish-haskell.yaml` is provided.
```
find . -name '*.hs' | xargs stylish-haskell -i
```
