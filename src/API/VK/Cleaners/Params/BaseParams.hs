module API.VK.Cleaners.Params.BaseParams where

import qualified API.VK.Structs.Exports as VKStructs
import qualified Logic.PureStructs as PureStructs

baseParams :: VKStructs.VKMessage -> [PureStructs.Params]
baseParams vkMsg = [PureStructs.ParamsNum "user_id" (VKStructs.from_id vkMsg)]
