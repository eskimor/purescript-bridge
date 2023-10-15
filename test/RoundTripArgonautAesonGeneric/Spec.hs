{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module RoundTripArgonautAesonGeneric.Spec where

import           Control.Exception (bracket)
import           Data.Aeson (FromJSON, ToJSON (toJSON), eitherDecode, encode,
                             fromJSON)
import           Data.ByteString.Lazy (hGetContents, stripSuffix)
import           Data.ByteString.Lazy.UTF8 (fromString, toString)
import           Data.List (isInfixOf)
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
import           Language.PureScript.Bridge (BridgePart, Language (..), SumType,
                                             argonautAesonGeneric, buildBridge,
                                             defaultBridge, equal, functor,
                                             genericShow, mkSumType, order, jsonHelper,
                                             writePSTypes, writePSTypesWith)
import           Language.PureScript.Bridge.TypeParameters (A)
import           RoundTripArgonautAesonGeneric.Types
import           System.Directory (removeDirectoryRecursive, removeFile,
                                   withCurrentDirectory)
import           System.Exit (ExitCode (ExitSuccess))
import           System.IO (BufferMode (..), hFlush, hGetLine, hPutStrLn,
                            hSetBuffering, stderr, stdout)
import           System.Process (CreateProcess (std_err, std_in, std_out),
                                 StdStream (CreatePipe), createProcess,
                                 getProcessExitCode, proc,
                                 readProcessWithExitCode, terminateProcess,
                                 waitForProcess)
import           Test.Hspec (Spec, around, aroundAll_, around_, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.HUnit (assertBool, assertEqual)
import           Test.QuickCheck (verbose)
import           Test.QuickCheck.Property (Testable (property))

myBridge :: BridgePart
myBridge = defaultBridge

-- test `argonaut-aeson-generic`
instancesToGenerate = equal
  . order
  . genericShow
  . order
  . argonautAesonGeneric

myTypes :: [SumType 'Haskell]
myTypes =
    [ instancesToGenerate $ mkSumType @TestData
    , instancesToGenerate $ mkSumType @TestSum
    , instancesToGenerate $ mkSumType @TestRecursiveA
    , instancesToGenerate $ mkSumType @TestRecursiveB
    , functor . instancesToGenerate $ mkSumType @(TestRecord A)
    , instancesToGenerate $ mkSumType @TestNewtype
    , instancesToGenerate $ mkSumType @TestNewtypeRecord
    , instancesToGenerate $ mkSumType @TestMultiInlineRecords
    , instancesToGenerate $ mkSumType @TestTwoFields
    , instancesToGenerate $ mkSumType @TestEnum
    , instancesToGenerate $ mkSumType @MyUnit
    ]

roundtripSpec :: Spec
roundtripSpec = do
    -- test `argonaut-aeson-generic`
    aroundAll_ withProject $
        describe "writePSTypesWith argonaut-aeson-generics" do
            it "should be buildable" do
                (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
                assertEqual (stdout <> stderr) exitCode ExitSuccess
            it "should not warn of unused packages buildable" do
                (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
                assertBool stderr $ not $ "[warn]" `isInfixOf` stderr
            around withApp $
                it "should produce aeson-compatible argonaut instances with argonaut-aeson-generics library" $
                    \(hin, hout, herr, hproc) -> verbose . property $ \testData -> do
                        let input = toString $ encode @TestData testData
                        hPutStrLn hin input
                        err <- hGetLine herr
                        output <- hGetLine hout

                        -- empty string signifies no error from Purescript process
                        assertEqual ("Error from Purescript, parsing: " <> input) "" err

                        -- compare the value parsed by Purescipt to the
                        -- source value in Haskell
                        assertEqual ("Mismatch between value sent to Purescript and value returned: " <> output) (Right testData)
                          . eitherDecode @TestData
                          $ fromString output

  where
    withApp = bracket runApp killApp
    runApp = do
        (Just hin, Just hout, Just herr, hproc) <-
            createProcess
                (proc "spago" ["run"])
                    { std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
        hSetBuffering hin LineBuffering
        hSetBuffering hout LineBuffering
        hSetBuffering herr LineBuffering
        -- flush stderr output from build
        _ <- hGetLine herr
        -- wait for initial log message
        _ <- hGetLine hout
        pure (hin, hout, herr, hproc)

    killApp (_, _, _, hproc) = terminateProcess hproc

    withProject :: IO () -> IO ()
    withProject runSpec =
        withCurrentDirectory "test/RoundTripArgonautAesonGeneric/app" $ generate *> runSpec

    generate :: IO ()
    generate = do
        writePSTypesWith
            "src"
            (buildBridge myBridge)
            myTypes
