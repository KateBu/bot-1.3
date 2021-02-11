module API.Telegram.Structs.UpdateErr where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)

data UpdatesError = UpdatesError
  { error_code :: Int,
    description :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON UpdatesError
