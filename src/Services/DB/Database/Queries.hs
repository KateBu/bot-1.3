module Services.DB.Database.Queries where

import qualified API.PureStructs.Exports as PureStructs
import qualified Config.Exports as Config
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Query)

userId :: Config.Config -> PureStructs.ChatID -> Query
userId (Config.VKBot _) chatId = "\'" <> "vk" <> (fromString . show) chatId <> "\'"
userId (Config.TBot _) chatId = "\'" <> "tel" <> (fromString . show) chatId <> "\'"

userIdText :: Config.Config -> PureStructs.ChatID -> T.Text
userIdText (Config.VKBot _) chatId = "vk" <> (T.pack . show) chatId
userIdText (Config.TBot _) chatId = "tel" <> (T.pack . show) chatId

findUserQuery :: Config.Config -> PureStructs.ChatID -> Query
findUserQuery config chatId =
  "SELECT repeats FROM UserRepeats WHERE userId = "
    <> userId config chatId

addUserQuery :: Query
addUserQuery = "INSERT INTO UserRepeats (userId, repeats) VALUES (?,?) returning userId, repeats"

updateUserQuery :: Query
updateUserQuery = "UPDATE UserRepeats SET repeats = ? WHERE userId = ? returning userId, repeats"
