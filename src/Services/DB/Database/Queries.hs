module Services.DB.Database.Queries where

import Data.String (IsString (fromString))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Query)
import qualified Environment.Exports as Env
import qualified Logic.Structs as PureStructs

userId :: Env.Config -> PureStructs.ChatID -> Query
userId (Env.VKBot _) chatId = "\'" <> "vk" <> (fromString . show) chatId <> "\'"
userId (Env.TBot _) chatId = "\'" <> "tel" <> (fromString . show) chatId <> "\'"

userIdText :: Env.Config -> PureStructs.ChatID -> T.Text
userIdText (Env.VKBot _) chatId = "vk" <> (T.pack . show) chatId
userIdText (Env.TBot _) chatId = "tel" <> (T.pack . show) chatId

findUserQuery :: Env.Config -> PureStructs.ChatID -> Query
findUserQuery config chatId =
  "SELECT repeats FROM UserRepeats WHERE userId = "
    <> userId config chatId

addUserQuery :: Query
addUserQuery = "INSERT INTO UserRepeats (userId, repeats) VALUES (?,?) returning userId, repeats"

updateUserQuery :: Query
updateUserQuery = "UPDATE UserRepeats SET repeats = ? WHERE userId = ? returning userId, repeats"
