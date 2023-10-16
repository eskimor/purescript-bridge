{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}


module MyLib (main) where

import           Prelude

import           Control.Lens (view)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Map.Lazy as Map
import           Data.Text (pack, unpack)
import           GHC.Generics
import           GHC.TypeLits
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment (lookupEnv)
import           Test.QuickCheck (Arbitrary (..), chooseEnum, oneof, resize,
                                  sized, generate)

import           Types (Baz (Baz), Foo (Foo), fooList, fooMap, fooMessage,
                        fooNumber, TestData(..), TestSum(..))
import qualified Types

type FooServer
  = "foo" :> (Get '[JSON] Foo
              :<|> ReqBody '[JSON] Foo :> Post '[JSON] NoContent
             )

foo :: IO Foo
foo = do
  testData :: TestData <- generate arbitrary
  testSum :: TestSum <- generate arbitrary
  return $ Foo
    (pack "Hello")
    123
    [10..20]
    (Map.fromList [(pack "foo", 2), (pack "bar", 3), (pack "baz", 3)])
    (Baz $ pack "hello")
    testSum
    testData

fooServer :: Server FooServer
fooServer = getFoo :<|> postFoo
  where
    getFoo = do
      fooValue <- liftIO foo
      liftIO $ putStrLn "Serving:"
      liftIO $ Char8.putStrLn $ AP.encodePretty fooValue
      return fooValue

    postFoo foo = do
      let
        logMsg = "Foo message: " <> (unpack $ view fooMessage foo)
          <> "\t Foo number: " <> (show (view fooNumber foo))
          <> "\t Foo list length: " <> (show . length $ view fooList foo)
          <> "\t Foo Map length: " <> (show . length $ view fooMap foo)
      liftIO . putStrLn $ "Received from client:"
      liftIO . putStrLn $ logMsg
      return NoContent

staticServer :: Server Raw
staticServer = serveDirectoryWebApp "static"

type ExampleServer = FooServer :<|> Raw

api :: Proxy ExampleServer
api = Proxy

main :: IO ()
main = do
  run 8080 . serve api $ fooServer :<|> staticServer
