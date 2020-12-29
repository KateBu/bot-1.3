module API.VK.Cleaners.IsObjFunctions where

import qualified API.VK.Structs as VKStructs 

isLink, isSticker, isMedia :: VKStructs.AObject -> Bool 
isLink (VKStructs.VKLink _) = True 
isLink _ = False 

isSticker (VKStructs.VKSticker _) = True 
isSticker _ = False 

isMedia (VKStructs.VKAudio _ _) = True 
isMedia (VKStructs.VKVideo _ _ _) = True 
isMedia (VKStructs.VKWall _ _) = True 
isMedia (VKStructs.VKMarket _ _) = True 
isMedia (VKStructs.VKPoll _ _) = True 
isMedia _ = False 