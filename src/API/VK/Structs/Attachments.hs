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
        Attachment attachType <$> (Link <$> link .: "url")
      "sticker" -> do
        sticker <- obj .: "sticker"
        Attachment attachType <$> (Sticker <$> sticker .: "sticker_id")
      "audio" -> do
        audio <- obj .: "audio"
        Attachment attachType
          <$> ( Audio <$> audio .: "id"
                  <*> audio .: "owner_id"
              )
      "video" -> do
        video <- obj .: "video"
        Attachment attachType
          <$> ( Video <$> video .: "id"
                  <*> video .: "owner_id"
                  <*> video .: "access_key"
              )
      "wall" -> do
        wall <- obj .: "wall"
        Attachment attachType
          <$> ( Wall <$> wall .: "id"
                  <*> wall .: "to_id"
              )
      "market" -> do
        market <- obj .: "market"
        Attachment attachType
          <$> ( Market <$> market .: "id"
                  <*> market .: "owner_id"
              )
      "poll" -> do
        poll <- obj .: "poll"
        Attachment attachType
          <$> ( Poll <$> poll .: "id"
                  <*> poll .: "owner_id"
              )
      _ -> pure $ Attachment attachType Unknown

type ItemID = Int

type OwnerID = Int

type AccessKey = T.Text

type Url = T.Text

data AObject
  = Link Url
  | Sticker ItemID
  | Audio ItemID OwnerID
  | Video ItemID OwnerID AccessKey
  | Wall ItemID OwnerID
  | Market ItemID OwnerID
  | Poll ItemID OwnerID
  | Unknown
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
