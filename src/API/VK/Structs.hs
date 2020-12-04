module API.VK.Structs where

import GHC.Generics
import qualified Data.Text as T 


data VKUpdates = VKUpdates {
    --    ts :: String
        updates :: [VKUpdInfo]
    } 
    | VKUpdateError {
        failed :: Int
        , curTs :: Maybe Integer 
    } deriving (Show)

data VKUpdInfo = VKUpdInfo
    {
        updType :: EventType
        , updObj :: Maybe VKObject
        , groupId :: Maybe Integer
    } deriving Show 

data VKObject = VKObject {
    vkMessage :: VKMessage
    , clientInfo :: ClientInfo
} deriving Show

data ClientInfo = ClientInfo {
    vkButtonActions :: [String]
    , vkKeyboard :: Bool
    , vkInlineKeyboard :: Bool
}deriving Show

data EventType = MsgNew | OtherEvent 
    deriving Show 

data VKMessage = VKMessage 
    {
        id :: Integer
    --    , date :: Integer 
        , peer_id :: Integer --reciever id 
        , from_id :: Integer --sender id
        , msgText :: Maybe T.Text
     {-   , random_id :: Maybe Integer
        , ref :: Maybe String 
        , ref_source :: Maybe String
        , attachments :: Maybe [Attachment]
        , important :: Maybe Bool
        , geo :: Maybe Geo
        , payload :: Maybe String 
        , keyboard :: Maybe Keyboard 
        , fwd_messages :: Maybe [VKMessage]
        , reply_message :: Maybe VKMessage
        , action :: Maybe Action 
        , admin_author_id ::Maybe Integer 
        , conversation_message_id :: Maybe Integer
        , is_cropped :: Maybe Bool
        , members_count :: Maybe Integer 
        , update_time :: Maybe Integer 
        , was_listened :: Maybe Bool 
        , pinned_at :: Maybe Bool -} 
    } deriving Show 

data Attachment = Attachment 
    {
        aType :: AType 
        , aObject :: AObject
    } deriving Show 

data AType = APhoto | AVideo | AAudio 
    | ADoc | ALink | AMarket | AMarketAlbum
    | AWall | AWallReply | ASticker | AGift 
    deriving Show 

data AObject = 
    VKPhoto 
        {
            phId :: Integer 
            , albumId :: Integer 
            , ownerId :: Integer 
            , userId :: Integer 
            , phText :: T.Text
            , phDate :: Integer 
            , phSizes :: [PhSizes]
            , phWidth :: Integer 
            , phHeight :: Integer 
        } deriving Show 

data PhSizes = PhSizes
    {
        url :: String 
        , sWidth :: Integer 
        , sHeight :: Integer 
        , sType :: String 
    } deriving Show 

data Geo = Geo 
    {
        gType :: String
    } deriving Show 

data Keyboard = Keyboard
    deriving Show

data Action = Action 
    deriving Show 


data VKResponse = VKResponse {
        key :: String
        , server :: String
        , currentTs :: String 
    } 
    | VKError {
        error_code :: Int 
        , error_msg :: T.Text
    } 
    | VKParseError deriving Show
    


data VKSend = VKSend {
    respPeerId :: Integer
    , respMessage :: Maybe T.Text
    , respRandomId :: Integer 
} deriving Show 

data VKResult = SendMsgScs {
        newTs :: Integer
    } 
    | SendMsgError {
        resError :: VKResultError
    } deriving Show 

data VKResultError = VKResultError {
    errCode :: Integer
    , errMsg :: T.Text
} deriving Show 