{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}


module MyLib (main) where

import Prelude

import Control.Lens (view)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (pack, unpack)
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import System.Environment (lookupEnv)

import Types (Foo(Foo), fooMessage, fooNumber)

type FooServer
  = "foo" :> (Get '[JSON] Foo
              :<|> ReqBody '[JSON] Foo :> Post '[JSON] NoContent
             )

fooServer :: Server FooServer
fooServer = getFoo :<|> postFoo
  where
    getFoo = return $ Foo (pack "Hello") 123
    postFoo foo = do
      let
        logMsg = "Foo message: " <> (unpack $ view fooMessage foo)
          <> "\t Foo number: " <> (show (view fooNumber foo))
      liftIO . putStrLn $ logMsg
      return NoContent

staticServer :: Server Raw
staticServer = serveDirectoryWebApp "static"

type ExampleServer = FooServer :<|> Raw

api :: Proxy ExampleServer
api = Proxy

main :: IO ()
main = run 8080 . serve api $ fooServer :<|> staticServer
