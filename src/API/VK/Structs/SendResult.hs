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

data VKResult
  = SendMsgSuccess SendSuccess
  | SendMsgError SendError
  deriving (Show)

newtype SendSuccess = SendSuccess
  { new_ts :: Int
  }
  deriving (Show)

newtype SendError = SendError
  { res_error :: VKResultError
  }
  deriving (Show)

instance FromJSON VKResult where
  parseJSON = withObject "VKResult" $ \obj -> do
    isError <- obj .:? "error"
    case isError of
      Just val -> pure $ SendMsgError (SendError val)
      Nothing -> SendMsgSuccess <$> (SendSuccess <$> obj .: "response")

data VKResultError = VKResultError
  { err_code :: Int,
    err_msg :: T.Text
  }
  deriving (Show)

instance FromJSON VKResultError where
  parseJSON (Object obj) =
    VKResultError
      <$> obj .: "error_code"
      <*> obj .: "error_msg"
  parseJSON _ = parseFail parseFailMessage
