# Development

## Example
See `example/readme.md`

## Spago
All PureScript code can be built from the root of the repository.
*Required*: Spago 0.93.44+

## Integration tests

Run `cabal test` in the root of the repository.

This includes:
* unit tests
* Round Trip integration tests for two libraries
  * json-helpers
  * argonaut-aeson-generic

## Nix Flake

*All requirements* (including Spago, Cabal, GHC) to build and test the library are provided in the Nix Flake.

Enter the development shell via `nix develop`.
