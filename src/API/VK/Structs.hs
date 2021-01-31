module API.VK.Structs
  ( module Structs,
  )
where

import API.VK.Structs.Buttons as Structs
  ( BtnAction (..),
    VKButtons (..),
    VKKeyBoard (..),
  )
import API.VK.Structs.LongPollResponse as Structs
  ( LongPollResponse (..),
    ResponseError (..),
    VKResponse (..),
  )
import API.VK.Structs.SendResult as Structs
  ( SendError (..),
    SendSuccess (..),
    VKResult (..),
    VKResultError (..),
  )
import API.VK.Structs.Updates as Structs
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
