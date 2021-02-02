module API.Telegram.Cleaners.MakeMbParams where

import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs

type Key = T.Text

type TextValue = T.Text

type BoolValue = Bool

type IntValue = Int

makeMaybeTextParams :: Key -> Maybe TextValue -> [PureStructs.Params]
makeMaybeTextParams _ Nothing = []
makeMaybeTextParams key (Just val) = pure $ PureStructs.ParamsText key val

makeMaybeBoolParams :: Key -> Maybe BoolValue -> [PureStructs.Params]
makeMaybeBoolParams _ Nothing = []
makeMaybeBoolParams key (Just val) = pure $ PureStructs.ParamsBool key val

makeMaybeNumParams :: Key -> Maybe IntValue -> [PureStructs.Params]
makeMaybeNumParams _ Nothing = []
makeMaybeNumParams key (Just val) = pure $ PureStructs.ParamsNum key val
