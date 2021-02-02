module API.Telegram.Cleaners.MakeMbParams where

import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

type Key = T.Text

makeMaybeTextParams :: Key -> Maybe T.Text -> [PureStructs.Params]
makeMaybeTextParams _ Nothing = []
makeMaybeTextParams key (Just val) = pure $ PureStructs.ParamsText key val

makeMaybeBoolParams :: Key -> Maybe Bool -> [PureStructs.Params]
makeMaybeBoolParams _ Nothing = []
makeMaybeBoolParams key (Just val) = pure $ PureStructs.ParamsBool key val

makeMaybeNumParams :: Key -> Maybe Int -> [PureStructs.Params]
makeMaybeNumParams _ Nothing = []
makeMaybeNumParams key (Just val) = pure $ PureStructs.ParamsNum key val
