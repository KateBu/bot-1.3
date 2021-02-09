module Wrapper.Functions.URL
  ( buildUrlBody,
    buildMultipartBody,
    isMultipart,
    buildHostPath,
    updateParam,
    mbSendOption,
  )
where

import qualified API.Telegram.URL as Telegram
import qualified API.VK.URL as VK
import Data.Aeson (encode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Environment.Exports as Env
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

mbSendOption :: Env.Config -> IO (Option 'Https)
mbSendOption vk@(Env.VKBot _) = do
  params <- VK.buildSendBasicParams vk
  pure $ mconcat (buildHttps <$> params)
mbSendOption _ = mempty

updateParam :: Env.Config -> [PureStructs.Params]
updateParam vk@(Env.VKBot _) = VK.buildGetUpdatesParams vk
updateParam telegram@(Env.TBot _) = Telegram.buildGetUpdateParams telegram

buildHostPath :: Env.Config -> WrapStructs.Method -> Maybe PureStructs.PureMessage -> Maybe (Url 'Https)
buildHostPath config WrapStructs.GetUpdate _ = buildUpdateHostPath config
buildHostPath config WrapStructs.Send (Just msg) = buildSendHostPath config msg
buildHostPath _ _ _ = Nothing

buildUpdateHostPath :: Env.Config -> Maybe (Url 'Https)
buildUpdateHostPath vk@(Env.VKBot _) = buildUrlScheme (VK.buildGetUpdatesHostPath vk)
buildUpdateHostPath telegram@(Env.TBot _) = buildUrlScheme (Telegram.buildGetUpdateHostPath telegram)

buildSendHostPath :: Env.Config -> PureStructs.PureMessage -> Maybe (Url 'Https)
buildSendHostPath (Env.VKBot _) _ = buildUrlScheme VK.buildSendHostPath
buildSendHostPath telegram@(Env.TBot _) msg =
  buildUrlScheme $
    Telegram.buildSendHostPath telegram (PureStructs.messageType msg)
