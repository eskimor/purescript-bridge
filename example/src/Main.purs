module Main where

import Prelude

import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just))
import Data.Lens (over, view, set)
import Data.Foldable (length)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Affjax.Web (get, post_)
import Affjax.ResponseFormat (json)
import Affjax.RequestBody as RequestBody
import Types (Foo, fooMessage, fooNumber, fooList, fooMap)

import Foreign.Object as Object

main :: Effect Unit
main = log "Hello, Purescript!" *> launchAff_ do
  -- "Foo" tests untagged JSON, i.e.:
  -- { "_fooMessage": "Hello", "_fooNumber": 123 }

  -- request a Foo
  fooResponse <- get json "/foo"
  for_ fooResponse \fooPayload -> do
    let
      efoo :: Either JsonDecodeError Foo
      efoo = genericDecodeAeson defaultOptions fooPayload.body
    case efoo of
      Left e -> liftEffect $ log $ "Error decoding Foo: " <> printJsonDecodeError e
      Right _ -> pure unit
    for_ efoo \foo -> do
      liftEffect do
        log $ "Foo message: " <> (view fooMessage foo)
          <> "\t Foo number: " <> (show $ view fooNumber foo)
          <> "\t Foo list length: "
          <> (show (length $ view fooList foo :: Int))
          <> "\t Foo map size: "
          <> (show (Object.size $ view fooMap foo :: Int))
      let
        -- modify the Foo received and send it back
        foo' = set fooMessage "Hola"
               $ over fooNumber (_+1)
               $ over fooList (\l -> l <> l)
               $ over fooMap (\o -> Object.insert "abc" 123 o)
               $ foo
        response = Just $ RequestBody.json $ genericEncodeAeson defaultOptions foo'
      post_ "/foo" response
