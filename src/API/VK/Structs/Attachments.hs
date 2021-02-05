module API.VK.Structs.Attachments where

import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    withObject,
    (.:),
  )
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import GHC.Generics (Generic)
import TextMessages.ParseFailMessage (parseFailMessage)

data Attachment = Attachment
  { attacnment_type :: T.Text,
    attachment_object :: AObject
  }
  deriving (Show)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \obj -> do
    attachType <- obj .: "type"
    case (attachType :: T.Text) of
      "link" -> do
        link <- obj .: "link"
        Attachment attachType <$> (VKLink <$> link .: "url")
      "sticker" -> do
        sticker <- obj .: "sticker"
        Attachment attachType <$> (VKSticker <$> sticker .: "sticker_id")
      "audio" -> do
        audio <- obj .: "audio"
        Attachment attachType
          <$> ( VKAudio <$> audio .: "id"
                  <*> audio .: "owner_id"
              )
      "video" -> do
        video <- obj .: "video"
        Attachment attachType
          <$> ( VKVideo <$> video .: "id"
                  <*> video .: "owner_id"
                  <*> video .: "access_key"
              )
      "wall" -> do
        wall <- obj .: "wall"
        Attachment attachType
          <$> ( VKWall <$> wall .: "id"
                  <*> wall .: "to_id"
              )
      "market" -> do
        market <- obj .: "market"
        Attachment attachType
          <$> ( VKMarket <$> market .: "id"
                  <*> market .: "owner_id"
              )
      "poll" -> do
        poll <- obj .: "poll"
        Attachment attachType
          <$> ( VKPoll <$> poll .: "id"
                  <*> poll .: "owner_id"
              )
      _ -> pure $ Attachment attachType VKUnknown

type ItemID = Int

type OwnerID = Int

type AccessKey = T.Text

type Url = T.Text

data AObject
  = VKLink Url
  | VKSticker ItemID
  | VKAudio ItemID OwnerID
  | VKVideo ItemID OwnerID AccessKey
  | VKWall ItemID OwnerID
  | VKMarket ItemID OwnerID
  | VKPoll ItemID OwnerID
  | VKUnknown
  deriving (Show)

newtype Geo = Geo
  { geo_coordinates :: Coordinates
  }
  deriving (Show)

instance FromJSON Geo where
  parseJSON (Object obj) =
    Geo
      <$> obj .: "coordinates"
  parseJSON _ = parseFail parseFailMessage

data Coordinates = Coordinates
  { latitude :: Double,
    longitude :: Double
  }
  deriving (Show, Generic)

instance FromJSON Coordinates
