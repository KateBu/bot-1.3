module Wrapper.Functions.URL
  ( buildUrlBody,
    buildMultipartBody,
    isMultipart,
    buildHostPath,
    updateParam,
    mbSendOption,
  )
where

import qualified API.Telegram.Data as TelData
import qualified API.VK.Data as VKData
import Data.Aeson (encode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Environment.Config.Exports as Config
import qualified Logic.Structs as PureStructs
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
import qualified Wrapper.Structs as WrapStructs

buildUrlScheme :: Maybe WrapStructs.HostPath -> Maybe (Url 'Https)
buildUrlScheme (Just (WrapStructs.HostPath host path)) = pure $ buildUrlScheme' (https host) path
  where
    buildUrlScheme' = foldl (/:)
buildUrlScheme _ = Nothing

buildUrlEncodedParam :: PureStructs.Params -> FormUrlEncodedParam
buildUrlEncodedParam (PureStructs.ParamsText key val) = key =: val
buildUrlEncodedParam (PureStructs.ParamsNum key val) = key =: val
buildUrlEncodedParam (PureStructs.ParamsDouble key val) = key =: val
buildUrlEncodedParam (PureStructs.ParamsBool key val) = key =: val
buildUrlEncodedParam (PureStructs.ParamsTextList _ _) = mempty
buildUrlEncodedParam (PureStructs.ParamsJSON _ _) = mempty

buildMultipartParam :: PureStructs.Params -> [LM.Part]
buildMultipartParam (PureStructs.ParamsText key val) = [LM.partLBS key (TE.encodeUtf8 $ TL.fromStrict val)]
buildMultipartParam (PureStructs.ParamsNum key val) = [LM.partLBS key (encode val)]
buildMultipartParam (PureStructs.ParamsDouble key val) = [LM.partLBS key (encode val)]
buildMultipartParam (PureStructs.ParamsBool key val) = [LM.partLBS key (encode val)]
buildMultipartParam (PureStructs.ParamsJSON key val) = [LM.partLBS key (encode val)]
buildMultipartParam (PureStructs.ParamsTextList key val) = [LM.partLBS key (encode val)]

buildUrlBody :: [PureStructs.Params] -> ReqBodyUrlEnc
buildUrlBody params = ReqBodyUrlEnc $ mconcat (buildUrlEncodedParam <$> params)

buildMultipartBody :: [PureStructs.Params] -> IO ReqBodyMultipart
buildMultipartBody params = reqBodyMultipart $ mconcat (mapM buildMultipartParam params)

isMultipart :: PureStructs.Params -> Bool
isMultipart (PureStructs.ParamsJSON _ _) = True
isMultipart (PureStructs.ParamsTextList _ _) = True
isMultipart _ = False

buildHttps :: PureStructs.Params -> Option 'Https
buildHttps (PureStructs.ParamsText key val) = key =: val
buildHttps (PureStructs.ParamsNum key val) = key =: val
buildHttps _ = mempty

mbSendOption :: Config.Config -> IO (Option 'Https)
mbSendOption vk@(Config.VKBot _) = do
  params <- VKData.sendBasicParams vk
  pure $ mconcat (buildHttps <$> params)
mbSendOption _ = mempty

updateParam :: Config.Config -> [PureStructs.Params]
updateParam vk@(Config.VKBot _) = VKData.updateParams vk
updateParam telegram@(Config.TBot _) = TelData.updateParams telegram

buildHostPath :: Config.Config -> WrapStructs.Method -> Maybe PureStructs.PureMessage -> Maybe (Url 'Https)
buildHostPath config WrapStructs.GetUpdate _ = buildUpdateHostPath config
buildHostPath config WrapStructs.Send (Just msg) = buildSendHostPath config msg
buildHostPath _ _ _ = Nothing

buildUpdateHostPath :: Config.Config -> Maybe (Url 'Https)
buildUpdateHostPath vk@(Config.VKBot _) = buildUrlScheme (VKData.updateHostPath vk)
buildUpdateHostPath telegram@(Config.TBot _) = buildUrlScheme (TelData.updateHostPath telegram)

buildSendHostPath :: Config.Config -> PureStructs.PureMessage -> Maybe (Url 'Https)
buildSendHostPath (Config.VKBot _) _ = buildUrlScheme VKData.sendHostPath
buildSendHostPath telegram@(Config.TBot _) msg =
  buildUrlScheme $
    TelData.sendHostPath telegram (PureStructs.messageType msg)
