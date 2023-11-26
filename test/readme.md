# Tests

There are RoundTrip tests for both the `json-helpers` library and the `argonaut-aeson-generic` library.

These have been separated into two directories, and the test specs are duplicated.

This is necessary because the generated PureScript takes its module name from the Haskell module which generates it. If both generated PureScript modules have the same module name, the root Spago project will fail to build because of module name duplication.
