module API.Telegram.Structs.Attachments.Location where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data TelLocation = TelLocation
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Generic, Show)

instance FromJSON TelLocation
