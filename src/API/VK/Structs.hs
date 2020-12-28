module API.VK.Structs (
    module Structs
    , module API.VK.Structs) where

import API.VK.VKStructs.Buttons as Structs
    ( BtnAction(..), VKButtons(..), VKKeyBoard(..) ) 

import API.VK.VKStructs.Updates as Structs
    ( AObject(..),
      AccessKey,
      Attachment(..),
      Coordinates(..),
      Geo(..),
      ItemID,
      OwnerID,
      Url,
      VKMessage(..),
      EventType(..),
      VKObject(..),
      VKUpdInfo(..),
      UpdateErr(..),
      Updates(..),
      VKUpdates(..) ) 
import API.VK.VKStructs.LongPollResponse as Structs
    ( LongPollResponse(..), ResponseError(..), VKResponse(..) ) 
import API.VK.VKStructs.SendResult as Structs
    ( VKResultError(..), SendError(..), SendSuccess(..), VKResult(..) ) 