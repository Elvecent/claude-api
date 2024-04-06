{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.Client where

import           Anthropic.Claude.API
import           Anthropic.Claude.Types
import           Data.Generics.Labels   ()
import           Data.Proxy
import           Servant.Client

type Version = String
type APIKey = String

getCompletion
  :: CompletionRequest
  -> Version
  -> APIKey
  -> ClientM CompletionResponse
getCompletion req version apiKey =
  client (Proxy @AnthropicAPI) req (Just version) (Just apiKey)
