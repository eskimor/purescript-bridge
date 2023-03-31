# purescript-bridge


[![Haskell library and example](https://github.com/eskimor/purescript-bridge/actions/workflows/haskell.yml/badge.svg)](https://github.com/eskimor/purescript-bridge/actions/workflows/haskell.yml) [![Purescript example](https://github.com/eskimor/purescript-bridge/actions/workflows/purescript.yml/badge.svg)](https://github.com/eskimor/purescript-bridge/actions/workflows/purescript.yml) [![Nix Flake](https://github.com/eskimor/purescript-bridge/actions/workflows/nix-flake.yml/badge.svg)](https://github.com/eskimor/purescript-bridge/actions/workflows/nix-flake.yml)



Translate your Haskell types to PureScript types. It should in theory work for almost all Haskell types, including type constructors!
You just have to instantiate it with dummy parameters from e.g. "Language.PureScript.Bridge.TypeParameters".

Data type translation is fully and easily customizable by providing your own `BridgePart` instances!

The latest version of this project requires **Purescript 0.15**.

## JSON encoding / decoding

For compatible JSON representations:

* On Haskell side:
  * Use [`aeson`](http://hackage.haskell.org/package/aeson)'s generic encoding/decoding with default options
* On Purescript side:
  * Use [`purescript-argonaut-aeson-generic >=0.4.1`](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic/0.4.1) ([GitHub](https://github.com/coot/purescript-argonaut-aeson-generic))
  * Or use [`purescript-foreign-generic`](https://pursuit.purescript.org/packages/purescript-foreign-generic).
    * [This branch](https://github.com/paf31/purescript-foreign-generic/pull/76) is updated for Purescript 0.15.

## Documentation

Usage of this library is documented in `Language.Purescript.Bridge`, with `writePSTypes` you should have everything to get started. Documentation can be found [here](https://www.stackage.org/nightly/package/purescript-bridge).

## Status

It works for my use case and is used in production. PRs for more `PSType`s definitions and bridges are very welcome! 

## Contributing

### Formatting the source code

This repo uses [`stylish-haskell`](https://github.com/haskell/stylish-haskell). `.stylish-haskell.yaml` is provided.
```
find . -name '*.hs' | xargs stylish-haskell -i
```
