module API.VK.Structs.LongPollResponse where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import qualified Data.Text as T

data Response
  = Response LongPollResponse
  | Error ResponseError
  | ParseError
  deriving (Show)

data LongPollResponse = LongPollResponse
  { key :: T.Text,
    server :: T.Text,
    current_ts :: String
  }
  deriving (Show)

data ResponseError = ResponseError
  { error_code :: Int,
    error_msg :: T.Text
  }
  deriving (Show)

instance FromJSON Response where
  parseJSON = withObject "Response" $ \obj -> do
    resp <- obj .:? "response"
    case resp of
      Just val ->
        Response
          <$> ( LongPollResponse <$> val .: "key"
                  <*> val .: "server"
                  <*> val .: "ts"
              )
      Nothing -> do
        err <- obj .:? "error"
        case err of
          Just val ->
            Error
              <$> ( ResponseError <$> val .: "error_code"
                      <*> val .: "error_msg"
                  )
          Nothing ->
            pure ParseError
