module Logic.PureStructs where

import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 
import Data.Aeson ( Value ) 
--import qualified Data.ByteString.Lazy as BSL 

import Logger.Logger ()


type UpdateID = Int
type ChatID = Int 
type MbCaption = Maybe T.Text

data MessageType = MTEmpty | MTUserCommand UCommand | MTCallbackQuery T.Text | MTCommon T.Text | NotImplemented 

data PureMessage = PureMessage 
    {
        messageType :: MessageType
        , updateID :: UpdateID 
        , mbChatID :: Maybe ChatID
        , mbParams :: Maybe [Params] 
    }

data UCommand = Help | Repeat 

data Params = ParamsText T.Text T.Text
        | ParamsTextLazy T.Text TL.Text
        | ParamsNum T.Text Int 
        | ParamsBool T.Text Bool 
        | ParamsJSON T.Text Value 
    --    | ParamsFile T.Text T.Text BSL.ByteString 
        deriving (Show, Eq)

buttons' :: [[PureButtons]]
buttons' = [[PureButtons "1" rep1]
    , [PureButtons "2" rep2]
    , [PureButtons "3" rep3]
    , [PureButtons "4" rep4]
    , [PureButtons "5" rep5]]

repeatText :: T.Text
repeatText = "Выберите количество повторов: "

newRepeatText :: Int -> T.Text
newRepeatText rep = "Установлено количество обновлений: " <> (T.pack . show) rep

rep1:: T.Text
rep1 = "/setRepetition1"

rep2:: T.Text
rep2 = "/setRepetition2"

rep3:: T.Text
rep3 = "/setRepetition3"

rep4:: T.Text
rep4 = "/setRepetition4"

rep5:: T.Text
rep5 = "/setRepetition5"

data HostPath = HostPath T.Text [T.Text] 
    deriving Show 

data PureButtons = PureButtons T.Text T.Text 
    deriving Show 

-- the code below will be removed soon
{-
data Command = Command 
    {
         chatID :: ChatID
        , text :: T.Text
    }

data Message = EmptyMessage UpdateID
    | UserCommand UpdateID Command
    | CommonMessage 
        {
            comMsgUid :: UpdateID
            , comMsgChid :: ChatID 
            , comMsg :: ComMessage
            , mbCaption :: MbCaption
        }
    | CallbackQuery UpdateID ChatID T.Text

data ComMessage = ComMessage
    {
        commonMsgType :: T.Text 
        , mbText :: Maybe T.Text
        , mbAnimationFileId :: Maybe T.Text
        , mbAudio :: Maybe PureAudio
        , mbDocFileId :: Maybe T.Text
        , mbPhotoFileIds :: Maybe [T.Text]
        , mbVideoFileId :: Maybe T.Text
        , mbVoiceFileId :: Maybe T.Text
        , mbContact :: Maybe PureContact
        , mbPoll :: Maybe PurePoll 
        , mbVenue :: Maybe PureVenue 
        , mbLocation :: Maybe PureLocation 
        , mbSticker :: Maybe PureSticker 
        , buttons :: Bool
    }deriving Show 

data PureAudio = PureAudio
    {
        audioFileId :: T.Text
        , mbAudioDuration :: Maybe Int 
        , mbAudioPerformer :: Maybe T.Text 
        , mbAudioTitle :: Maybe T.Text 
    }deriving Show 

data PureContact = PureContact 
    {
        contactPhoneNumber :: T.Text
        , contactFirstName :: T.Text
        , mbContactLastName :: Maybe T.Text
        , mbContactVCard :: Maybe T.Text
    }deriving Show 

data PurePoll = PurePoll
    {
        pollQuestion :: T.Text
        , pollOptions :: [(T.Text, Int)]
        , mbPollAnonymous :: Maybe Bool
        , mbPollType :: Maybe T.Text
        , mbPollAllowsMultiAnswers :: Maybe Bool
        , mbPollCorrectId :: Maybe Int
        , mbPollExplanation :: Maybe T.Text
        , mbPollOpenPeriod :: Maybe Int 
        , mbPollCloseDate :: Maybe Int 
        , mbPollIsClosed :: Maybe Bool
    } deriving Show 

data PureVenue = PureVenue 
    {
        venueLat :: Double
        , venueLong :: Double 
        , venueTitle :: T.Text
        , venueAddress :: T.Text
    }deriving Show 

data PureLocation = PureLocation 
    {
        locationLat :: Double 
        , locationLong :: Double 
    }deriving Show 

data PureSticker = PureSticker 
    {
        stickerFileId :: T.Text
        , stickerIsAnimated :: Bool 
    }deriving Show 



getMsgUid :: Message -> UpdateID
getMsgUid (UserCommand uid _) = uid 
getMsgUid (CommonMessage uid _ _ _) = uid 
getMsgUid (EmptyMessage uid) = uid 
getMsgUid (CallbackQuery uid _ _) = uid 

getContentType :: Message -> T.Text
getContentType (CommonMessage _ _ cMsg _) = commonMsgType cMsg 
getContentType _ = ""

defaultComMsg :: ComMessage 
defaultComMsg = ComMessage "" 
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    False

cMsgToText :: ComMessage -> T.Text
cMsgToText cMsg = getMaybeText (mbText cMsg)

getMaybeText :: Maybe T.Text -> T.Text
getMaybeText Nothing = ""
getMaybeText (Just txt) = "Text message: " <> txt 

getMsgType :: Message -> T.Text
getMsgType (EmptyMessage _) = "EmptyMessage"
getMsgType (UserCommand _ _) = "UserCommand"
getMsgType (CommonMessage _ _ _ _) = "CommonMessage"
getMsgType (CallbackQuery _ _ _) = "CallbackQuery"

-}