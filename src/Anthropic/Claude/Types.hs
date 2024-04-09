{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoFieldSelectors       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Anthropic.Claude.Types
  ( CompletionRequest (..),
    CompletionResponse (..),
    Message (..),
    MessageRole (..),
    MessageContent (..),
    ContentBlock (..),
    StopReason (..),
    Usage (..),
    ImageSource (..),
  )
where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Utils
import           Data.Default
import           Data.Text        (Text)
import           GHC.Generics

data MessageRole = User | Assistant
  deriving (Generic, Show, Eq)

instance Default MessageRole where
  def = User

data ImageSource = ImageSource
  { type_     :: Text,
    mediaType :: Text,
    data_     :: Text
  }
  deriving (Generic, Show, Eq)

instance Default ImageSource where
  def = ImageSource {
    type_ = "base64",
    mediaType = "",
    data_ = ""
  }

data MessageContent
  = TextContent Text
  | ImageContent ImageSource
  deriving (Generic, Show, Eq)

data Message = Message
  { role    :: MessageRole,
    content :: [MessageContent]
  }
  deriving (Generic, Show, Eq)

instance Default Message where
  def = Message {
    role = def,
    content = []
  }

data CompletionRequest = CompletionRequest
  { model         :: Text,
    maxTokens     :: Integer,
    messages      :: [Message],
    system        :: Maybe Text,
    stopSequences :: Maybe [Text],
    temperature   :: Maybe Double,
    topP          :: Maybe Double,
    topK          :: Maybe Integer,
    metadata      :: Maybe Value
  }
  deriving (Generic, Show, Eq)

instance Default CompletionRequest where
  def = CompletionRequest {
    model = "",
    maxTokens = 0,
    messages = [],
    system = Nothing,
    stopSequences = Nothing,
    temperature = Nothing,
    topP = Nothing,
    topK = Nothing,
    metadata = Nothing
  }

newtype ContentBlock = ContentBlock
  { text :: Text
  }
  deriving (Generic, Show, Eq)

data StopReason
  = EndTurn
  | MaxTokens
  | StopSequence
  deriving (Generic, Show, Eq)

data Usage = Usage
  { inputTokens  :: Integer,
    outputTokens :: Integer
  }
  deriving (Generic, Show, Eq)

data CompletionResponse = CompletionResponse
  { id           :: Text,
    role         :: MessageRole,
    content      :: [ContentBlock],
    model        :: Text,
    stopReason   :: Maybe StopReason,
    stopSequence :: Maybe Text,
    usage        :: Usage
  }
  deriving (Generic, Show, Eq)

$(mconcat
  [ deriveJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' . dropSuffix "_"
        } ''ImageSource
  , deriveJSON
      defaultOptions{constructorTagModifier = camelTo2 '_'} ''MessageRole
  ]
 )

instance ToJSON MessageContent where
  toJSON (TextContent t)  = object ["text" .= t, "type" .= ("text" :: Text)]
  toJSON (ImageContent s) = object ["source" .= s, "type" .= ("image" :: Text)]

instance FromJSON MessageContent where
  parseJSON = withObject "MessageContent" $ \o -> do
    t <- o .: "type"
    case t :: Text of
      "text"  -> TextContent <$> o .: "text"
      "image" -> ImageContent <$> o .: "source"
      _       -> fail "Unknown message content type"

$(mconcat
  [ deriveJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_' } ''Message
  , deriveJSON
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        , omitNothingFields = True
        } ''CompletionRequest
  ]
 )

$(mconcat
  [ deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Usage
  , deriveJSON defaultOptions{constructorTagModifier = camelTo2 '_'} ''StopReason
  , deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''ContentBlock
  , deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''CompletionResponse
  ]
 )
