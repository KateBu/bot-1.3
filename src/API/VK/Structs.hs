module API.VK.Structs
  ( module Structs,
    module API.VK.Structs,
  )
where

import API.VK.VKStructs.Buttons as Structs
  ( BtnAction (..),
    VKButtons (..),
    VKKeyBoard (..),
  )
import API.VK.VKStructs.LongPollResponse as Structs
  ( LongPollResponse (..),
    ResponseError (..),
    VKResponse (..),
  )
import API.VK.VKStructs.SendResult as Structs
  ( SendError (..),
    SendSuccess (..),
    VKResult (..),
    VKResultError (..),
  )
import API.VK.VKStructs.Updates as Structs
  ( AObject (..),
    AccessKey,
    Attachment (..),
    Coordinates (..),
    EventType (..),
    Geo (..),
    ItemID,
    OwnerID,
    UpdateErr (..),
    Updates (..),
    Url,
    VKMessage (..),
    VKObject (..),
    VKUpdInfo (..),
    VKUpdates (..),
  )
