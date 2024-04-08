{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens
import           Control.Monad           (when)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Default
import           Data.Generics.Labels
import           Data.Typeable
import           Network.HTTP.Client.TLS
import           Servant.Client
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options
import           Text.RawString.QQ

import           Anthropic.Claude.Client

main :: IO ()
main = defaultMainWithIngredients ings tests
  where
    ings = includingOptions [Option (Proxy :: Proxy LiveTest)] : defaultIngredients

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup "Unit tests"
  [ requestEncodeDecode
  , responseDecodeEncodeDecode
  , requestSend
  ]

data LiveTest = LiveTest Bool
  deriving (Eq, Ord, Typeable)

instance IsOption LiveTest where
  defaultValue = LiveTest False
  parseValue = Just . LiveTest . read
  optionName = pure "live"
  optionHelp = pure "Run a live test, reading the API key from CLAUDE_API_KEY"
  optionCLParser = flagCLParser Nothing (LiveTest True)

keyPresent :: TestTree
keyPresent = testCase "keyPresent" $ do
  key <- lookupEnv "CLAUDE_API_KEY"
  assertBool "key is present" (key /= Nothing)

testRequest :: CompletionRequest
testRequest = def
  & #model .~ "claude-3-haiku-20240307"
  & #maxTokens .~ 10
  & #messages .~
    [ def
      & #role .~ def
      & #content .~ [TextContent "Hello, Claude"]
    ]

testResponse :: ByteString
testResponse = [r|{
  "content": [
    {
      "text": "Hi! My name is Claude.",
      "type": "text"
    }
  ],
  "id": "msg_013Zva2CMHLNnXjNJJKqJ2EF",
  "model": "claude-3-opus-20240229",
  "role": "assistant",
  "stop_reason": "end_turn",
  "stop_sequence": null,
  "type": "message",
  "usage": {
    "input_tokens": 10,
    "output_tokens": 25
  }
}|]

requestEncodeDecode :: TestTree
requestEncodeDecode = testCase "requestEncodeDecode" $ do
  let encoded = encode testRequest
  let decoded = decode encoded :: Maybe CompletionRequest
  assertBool "encode/decode" (decoded == Just testRequest)

responseDecodeEncodeDecode :: TestTree
responseDecodeEncodeDecode = testCase "responseDecodeEncode" $ do
  let decoded = decode testResponse :: Maybe CompletionResponse
  let encoded = encode decoded
  let decoded' = decode encoded
  assertBool "decode/encode/decode" (decoded == decoded')

requestSend :: TestTree
requestSend = askOption $ \(LiveTest live) -> testCase "testRequest" $ do
  mKey <- lookupEnv "CLAUDE_API_KEY"
  let version = "2023-06-01"
  case mKey of
    Nothing -> assertBool "skip" True
    Just key -> when live $ do
      manager <- newTlsManager
      url <- parseBaseUrl "https://api.anthropic.com"
      let env = mkClientEnv manager url
      resp <- runClientM (getCompletion testRequest version key) env
      case resp of
        Left err -> assertFailure (show err)
        Right _  -> assertBool "response" True
