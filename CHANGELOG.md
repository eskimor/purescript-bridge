# Changelog for purescript-bridge

## 0.16.0.0 (unreleased)
* merge https://github.com/input-output-hk/purescript-bridge
* support https://github.com/input-output-hk/purescript-bridge-json-helpers
* `CodeGenSwitches` no longer used
* `Proxy` no longer used
* `Printer` uses `Leijen.Text` instead of `Text`
* `genericShow` PureScript instance from IOHK
  * I think the same as https://github.com/eskimor/purescript-bridge/pull/85
    * PR 85 has been replaced with IOHK's implementation
* some unit tests replaced with IOHK's tests
* all printer imports are combined into one function `instanceToImportLines` instead of being divided between two modules
* excellent `RoundTrip` tests implemented by IOHK
  * greatly improves test coverage for both Argonaut and `json-helpers` and exposes issues
