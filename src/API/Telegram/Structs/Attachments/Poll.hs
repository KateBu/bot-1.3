module API.Telegram.Structs.Attachments.Poll where

import TextMessages.ParseFailMessage (parseFailMessage)
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    (.:),
    (.:?),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T

data TelPoll = TelPoll
  { question :: T.Text,
    poll_options :: [PollOptions],
    is_anonymous :: Maybe Bool,
    poll_type :: Maybe T.Text,
    allows_multiple_answers :: Maybe Bool,
    correct_option_id :: Maybe Int,
    explanation :: Maybe T.Text,
    open_period :: Maybe Int,
    close_date :: Maybe Int,
    is_closed :: Maybe Bool
  }
  deriving (Show)

instance FromJSON TelPoll where
  parseJSON (Object v) =
    TelPoll <$> v .: "question"
      <*> v .: "options"
      <*> v .:? "is_anonymous"
      <*> v .:? "type"
      <*> v .:? "allows_multiple_answers"
      <*> v .:? "correct_option_id"
      <*> v .:? "explanation"
      <*> v .:? "open_period"
      <*> v .:? "close_date"
      <*> v .:? "is_closed"
  parseJSON _ = parseFail parseFailMessage

data PollOptions = PollOptions
  { poll_option :: T.Text,
    voter_count :: Int
  }
  deriving (Show)

instance FromJSON PollOptions where
  parseJSON (Object v) =
    PollOptions <$> v .: "text"
      <*> v .: "voter_count"
  parseJSON _ = parseFail parseFailMessage
