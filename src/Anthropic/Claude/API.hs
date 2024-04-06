{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Anthropic.Claude.API where

import           Anthropic.Claude.Types

import           Servant.API

type AnthropicAPI =
  "v1"
    :> "messages"
    :> ReqBody '[JSON] CompletionRequest
    :> Header "anthropic-version" String
    :> Header "x-api-key" String
    :> Post '[JSON] CompletionResponse
