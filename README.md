## Usage example

```haskell
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Anthropic.Claude.Client
import           Control.Lens
import           Data.Default
import           Network.HTTP.Client.TLS
import           Servant.Client

exampleRequest :: CompletionRequest
exampleRequest = def
  & #model .~ "claude-3-haiku-20240307"
  & #maxTokens .~ 10
  & #messages .~
    [ def
      & #role .~ def
      & #content .~ [TextContent "Hello, Claude"]
    ]

sendExampleRequest :: IO ()
sendExampleRequest = do
  let version = "2023-06-01"
  let apiKey = "<REDACTED>"
  manager <- newTlsManager
  url <- parseBaseUrl "https://api.anthropic.com"
  let env = mkClientEnv manager url
  resp <- runClientM (getCompletion exampleRequest version apiKey) env
  print resp
```
