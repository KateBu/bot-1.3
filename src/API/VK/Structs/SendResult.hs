module API.VK.Structs.SendResult where

import API.Messages (parseFailMessage)
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data VKResult
  = SendMsgScs SendSuccess
  | SendMsgError SendError
  deriving (Show)

newtype SendSuccess = SendSuccess
  { newTs :: Int
  }
  deriving (Show)

newtype SendError = SendError
  { resError :: VKResultError
  }
  deriving (Show)

instance FromJSON VKResult where
  parseJSON = withObject "VKResult" $ \obj -> do
    isError <- obj .:? "error"
    case isError of
      Just val -> pure $ SendMsgError (SendError val)
      Nothing -> SendMsgScs <$> (SendSuccess <$> obj .: "response")

data VKResultError = VKResultError
  { errCode :: Int,
    errMsg :: T.Text
  }
  deriving (Show)

instance FromJSON VKResultError where
  parseJSON (Object obj) =
    VKResultError
      <$> obj .: "error_code"
      <*> obj .: "error_msg"
  parseJSON _ = parseFail parseFailMessage
