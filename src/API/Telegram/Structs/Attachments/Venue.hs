module API.Telegram.Structs.Attachments.Venue where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import TextMessages.ParseFailMessage (parseFailMessage)

data Venue = Venue
  { venue_latitude :: Double,
    venue_longitude :: Double,
    venue_title :: T.Text,
    venue_address :: T.Text
  }
  deriving (Show)

instance FromJSON Venue where
  parseJSON (Object v) =
    Venue <$> v .: "latitude"
      <*> v .: "longitude"
      <*> v .: "title"
      <*> v .: "address"
  parseJSON _ = parseFail parseFailMessage
