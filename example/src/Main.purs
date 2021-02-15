module Main where

import Prelude

import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Data.Lens (over, view, set)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Affjax (get, post_)
import Affjax.ResponseFormat (json)
import Affjax.RequestBody as RequestBody

import Types (Foo, fooMessage, fooNumber)

main :: Effect Unit
main = log "Hello, Purescript!" *> launchAff_ do
  fooResponse <- get json "/foo"
  for_ fooResponse \fooPayload -> do
    let
      efoo :: Either String Foo
      efoo = genericDecodeAeson defaultOptions fooPayload.body
    for_ efoo \foo -> do
      liftEffect do
        log $ "Foo message: " <> (view fooMessage foo)
          <> "\t Foo number: " <> (show $ view fooNumber foo)
      let
        foo' = set fooMessage "Hola" $ over fooNumber (_+1) foo
        response = Just $ RequestBody.json $ genericEncodeAeson defaultOptions foo'
      post_ "/foo" response
