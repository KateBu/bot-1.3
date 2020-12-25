module Logic.PureStructs where

import qualified Data.Text as T 
import Data.Aeson ( Value )
import Logger.Logger ()

type UpdateID = Int
type ChatID = Int 
type MbCaption = Maybe T.Text

data MessageType = MTEmpty 
    | MTUserCommand UCommand 
    | MTCallbackQuery T.Text 
    | MTCommon T.Text 
    deriving (Show, Eq)

data PureMessage = PureMessage 
    {
        messageType :: MessageType
        , updateID :: UpdateID 
        , mbChatID :: Maybe ChatID
        , mbParams :: Maybe [Params] 
    } deriving (Show, Eq)

data UCommand = Help | Repeat 
    deriving (Show, Eq)

data Params = ParamsText T.Text T.Text
        | ParamsTextList T.Text [T.Text] 
        | ParamsNum T.Text Int  
        | ParamsDouble T.Text Double
        | ParamsBool T.Text Bool 
        | ParamsJSON T.Text Value 
        deriving (Show, Eq)

data HostPath = HostPath T.Text [T.Text]  
    deriving Show 

data PureButtons = PureButtons T.Text T.Text
    deriving Show   

buttons' :: [[PureButtons]]
buttons' = [[PureButtons "1" rep1]
    , [PureButtons "2" rep2]
    , [PureButtons "3" rep3]
    , [PureButtons "4" rep4]
    , [PureButtons "5" rep5]]

repeatText :: T.Text
repeatText = "Выберите количество повторов: "

newRepeatText :: Int -> T.Text
newRepeatText rep = "Установлено количество повторов: " <> (T.pack . show) rep

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

getNewRep :: T.Text -> Int 
getNewRep txt 
    | txt == rep1 = 1 
    | txt == rep2 = 2 
    | txt == rep3 = 3 
    | txt == rep4 = 4 
    | otherwise = 5 

