module API.Telegram.Functions.Params where

import qualified Data.Text as T
import qualified Logic.Structs as PureStructs

type Key = T.Text

buildTextParams :: Key -> Maybe T.Text -> [PureStructs.Params]
buildTextParams _ Nothing = []
buildTextParams key (Just val) = pure $ PureStructs.ParamsText key val

buildBoolParams :: Key -> Maybe Bool -> [PureStructs.Params]
buildBoolParams _ Nothing = []
buildBoolParams key (Just val) = pure $ PureStructs.ParamsBool key val

buildNumParams :: Key -> Maybe Int -> [PureStructs.Params]
buildNumParams _ Nothing = []
buildNumParams key (Just val) = pure $ PureStructs.ParamsNum key val
