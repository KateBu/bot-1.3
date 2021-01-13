module API.Wrapper.WrapFunctions where

import qualified API.Telegram.TelData as TelData
import qualified API.VK.VKData as VKData
import qualified API.Wrapper.WrapStructs as WrapStructs
import qualified Config.Config as Config
import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Logic.PureStructs as PureStructs
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Req
  ( FormUrlEncodedParam,
    Option,
    ReqBodyMultipart,
    ReqBodyUrlEnc (..),
    Scheme (Https),
    Url,
    https,
    reqBodyMultipart,
    (/:),
    (=:),
  )

hostPathToUrlScheme :: Maybe WrapStructs.HostPath -> Maybe (Url 'Https)
hostPathToUrlScheme (Just (WrapStructs.HostPath hpHost hpPath)) = pure $ makeUrlScheme (https hpHost) hpPath
hostPathToUrlScheme _ = Nothing

makeUrlScheme :: Url 'Https -> [T.Text] -> Url 'Https
makeUrlScheme acc [] = acc
makeUrlScheme acc (x : xs) = makeUrlScheme (acc /: x) xs

paramToUrl :: PureStructs.Params -> FormUrlEncodedParam
paramToUrl (PureStructs.ParamsText key val) = key =: val
paramToUrl (PureStructs.ParamsNum key val) = key =: val
paramToUrl (PureStructs.ParamsDouble key val) = key =: val
paramToUrl (PureStructs.ParamsBool key val) = key =: val
paramToUrl (PureStructs.ParamsTextList _ _) = mempty
paramToUrl (PureStructs.ParamsJSON _ _) = mempty

paramToMultipart :: PureStructs.Params -> [LM.Part]
paramToMultipart (PureStructs.ParamsText key val) = [LM.partLBS key (TE.encodeUtf8 $ TL.fromStrict val)]
paramToMultipart (PureStructs.ParamsNum key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsDouble key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsBool key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsJSON key val) = [LM.partLBS key (encode val)]
paramToMultipart (PureStructs.ParamsTextList key val) = [LM.partLBS key (encode val)]

paramsToUrlBody :: [PureStructs.Params] -> ReqBodyUrlEnc
paramsToUrlBody params = ReqBodyUrlEnc $ mconcat (paramToUrl <$> params)

paramsToMultipartBody :: [PureStructs.Params] -> IO ReqBodyMultipart
paramsToMultipartBody params = reqBodyMultipart $ mconcat (mapM paramToMultipart params)

isMultipart :: PureStructs.Params -> Bool
isMultipart (PureStructs.ParamsJSON _ _) = True
isMultipart (PureStructs.ParamsTextList _ _) = True
isMultipart _ = False

paramsToHttps :: PureStructs.Params -> Option 'Https
paramsToHttps (PureStructs.ParamsText key val) = key =: val
paramsToHttps (PureStructs.ParamsNum key val) = key =: val
paramsToHttps _ = mempty

mbSendOption :: Config.BotType -> IO (Option 'Https)
mbSendOption vk@(Config.VKBot _) = do
  params <- VKData.sendBasicParams vk
  pure $ (mconcat (paramsToHttps <$> params))
mbSendOption _ = mempty

updateParam :: Config.BotType -> [PureStructs.Params]
updateParam vk@(Config.VKBot _) = VKData.updateParams vk
updateParam tel@(Config.TBot _) = TelData.updateParams tel

mkHostPath :: Config.Config -> WrapStructs.Method -> Maybe PureStructs.PureMessage -> Maybe (Url 'Https)
mkHostPath config WrapStructs.Update _ = mkUpdHostPath (Config.botType config)
mkHostPath config WrapStructs.Send (Just msg) = mkSndHostPath (Config.botType config) msg
mkHostPath _ _ _ = Nothing

mkUpdHostPath :: Config.BotType -> Maybe (Url 'Https)
mkUpdHostPath vk@(Config.VKBot _) = hostPathToUrlScheme (VKData.updateHostPath vk)
mkUpdHostPath tel@(Config.TBot _) = hostPathToUrlScheme (TelData.updateHostPath tel)

mkSndHostPath :: Config.BotType -> PureStructs.PureMessage -> Maybe (Url 'Https)
mkSndHostPath (Config.VKBot _) _ = hostPathToUrlScheme VKData.sendHostPath
mkSndHostPath tel@(Config.TBot _) msg =
  hostPathToUrlScheme $
    (TelData.sendHostPath tel (PureStructs.messageType msg))
