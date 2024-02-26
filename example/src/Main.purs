module Main where

import Prelude

import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat (json)
import Affjax.Web (get, post_)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Either (Either(Left, Right))
import Data.Foldable (length)
import Data.Lens (over, view, set)
import Data.Maybe (Maybe(Just))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (empty)
import Foreign.Object as Object
import Argonaut.Types (Baz(Baz), Foo(Foo), TestData(..), TestSum(..), fooMessage, fooNumber, fooList, fooMap, fooTestSum, fooTestData)

testFoo = Foo
  { _fooMessage: "foo"
  , _fooE: Left "foo"
  , _fooNumber: 1
  , _fooList: [1,2,3]
  , _fooMap: empty
  , _fooBaz: Baz { _bazMessage: "baz" }
  , _fooTestSum: Nullary
  , _fooTestData: TEither "foo"
  }

main :: Effect Unit
main = log "Hello, PureScript!" *> launchAff_ do
  -- request a Foo; decode with ArgonautAesonGeneric
  fooResponse <- get json "/foo"
  for_ fooResponse \fooPayload -> do
    liftEffect $ log $ "Decode and Encode with ArgonautAesoGeneric"
    let
      eArgonautDecodedFoo :: Either JsonDecodeError Foo
      eArgonautDecodedFoo = genericDecodeAeson defaultOptions fooPayload.body
    case eArgonautDecodedFoo of
      Left e -> liftEffect $ log $ "Error decoding Foo: " <> printJsonDecodeError e
      Right _ -> pure unit
    for_ eArgonautDecodedFoo \argonautDecodedFoo -> do
      liftEffect do
        log $ "Foo message: " <> (view fooMessage argonautDecodedFoo)
        log $ "Foo number: " <> (show $ view fooNumber argonautDecodedFoo)
        log $ "Foo list length: " <> (show (length $ view fooList argonautDecodedFoo :: Int))
        log $ "Foo map size: " <> (show (Object.size $ view fooMap argonautDecodedFoo :: Int))
        log $ "Foo test sum: " <> show (view fooTestSum argonautDecodedFoo)
        log $ "Foo test data: " <> show (view fooTestData argonautDecodedFoo)
      let
        -- modify the Foo received and send it back
        modifiedFoo = set fooMessage "Hello from ArgonautAesonGeneric"
               $ over fooNumber (_+1)
               $ over fooList (\l -> l <> l)
               $ over fooMap (\o -> Object.insert "abc" 123 o)
               $ argonautDecodedFoo
        response = Just $ RequestBody.json $ genericEncodeAeson defaultOptions modifiedFoo
      post_ "/foo" response

  -- request a Foo; decode with JsonHelpers
  fooResponse <- get json "/foo"
  for_ fooResponse \fooPayload -> do
    liftEffect $ log $ "Decode and Encode with JsonHelpers"
    let
      eJsonHelpersDecodedFoo :: Either JsonDecodeError Foo
      eJsonHelpersDecodedFoo = decodeJson fooPayload.body
    case eJsonHelpersDecodedFoo of
      Left e -> liftEffect $ log $ "Error decoding Foo: " <> printJsonDecodeError e
      Right _ -> pure unit
    for_ eJsonHelpersDecodedFoo \jsonHelpersFoo -> do
      liftEffect do
        log $ "Foo message: " <> (view fooMessage jsonHelpersFoo)
        log $ "Foo number: " <> (show $ view fooNumber jsonHelpersFoo)
        log $ "Foo list length: " <> (show (length $ view fooList jsonHelpersFoo :: Int))
        log $ "Foo map size: " <> (show (Object.size $ view fooMap jsonHelpersFoo :: Int))
        log $ "Foo test sum: " <> show (view fooTestSum jsonHelpersFoo)
        log $ "Foo test data: " <> show (view fooTestData jsonHelpersFoo)
      let
        -- modify the Foo received and send it back
        modifiedFoo = set fooMessage "Hello from JsonHelpers"
               $ over fooNumber (_+1)
               $ over fooList (\l -> l <> l)
               $ over fooMap (\o -> Object.insert "abc" 123 o)
               $ jsonHelpersFoo
        response = Just $ RequestBody.json $ encodeJson modifiedFoo
      post_ "/foo" response
