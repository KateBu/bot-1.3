module API.VK.Structs.LongPollResponse where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import qualified Data.Text as T

data VKResponse
  = VKResponse LongPollResponse
  | VKError ResponseError
  | VKParseError
  deriving (Show)

data LongPollResponse = LongPollResponse
  { key :: T.Text,
    server :: T.Text,
    currentTs :: String
  }
  deriving (Show)

data ResponseError = ResponseError
  { errorCode :: Int,
    errorMsg :: T.Text
  }
  deriving (Show)

instance FromJSON VKResponse where
  parseJSON = withObject "VKResponse" $ \obj -> do
    resp <- obj .:? "response"
    case resp of
      Just val ->
        VKResponse
          <$> ( LongPollResponse <$> val .: "key"
                  <*> val .: "server"
                  <*> val .: "ts"
              )
      Nothing -> do
        err <- obj .:? "error"
        case err of
          Just val ->
            VKError
              <$> ( ResponseError <$> val .: "error_code"
                      <*> val .: "error_msg"
                  )
          Nothing ->
            pure VKParseError
