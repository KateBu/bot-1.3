module API.Telegram.Cleaners.MakeMbParams where

import qualified Data.Text as T
import qualified Logic.PureStructs as PureStructs 

makeMaybeTextParams ::  T.Text -> Maybe T.Text -> [PureStructs.Params]
makeMaybeTextParams _ Nothing = [] 
makeMaybeTextParams key (Just val) = pure $ PureStructs.ParamsText key val

makeMaybeBoolParams :: T.Text -> Maybe Bool -> [PureStructs.Params]
makeMaybeBoolParams _ Nothing = []
makeMaybeBoolParams key (Just val) = pure $ PureStructs.ParamsBool key val

makeMaybeNumParams :: T.Text -> Maybe Int -> [PureStructs.Params]
makeMaybeNumParams _ Nothing = []
makeMaybeNumParams key (Just val) = pure $ PureStructs.ParamsNum key val