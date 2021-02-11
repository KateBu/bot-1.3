module API.VK.Structs.SendResult where

import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data Result
  = SendMsgSuccess SendSuccess
  | SendMsgError SendError
  deriving (Show)

newtype SendSuccess = SendSuccess
  { new_ts :: Int
  }
  deriving (Show)

newtype SendError = SendError
  { res_error :: ResultError
  }
  deriving (Show)

instance FromJSON Result where
  parseJSON = withObject "Result" $ \obj -> do
    isError <- obj .:? "error"
    case isError of
      Just val -> pure $ SendMsgError (SendError val)
      Nothing -> SendMsgSuccess <$> (SendSuccess <$> obj .: "response")

data ResultError = ResultError
  { err_code :: Int,
    err_msg :: T.Text
  }
  deriving (Show)

instance FromJSON ResultError where
  parseJSON (Object obj) =
    ResultError
      <$> obj .: "error_code"
      <*> obj .: "error_msg"
  parseJSON _ = parseFail parseFailMessage
