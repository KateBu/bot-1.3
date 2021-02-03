module Services.DB.Database.Queries where

import qualified Config.Exports  as Config
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Query)
import qualified Logic.PureStructs as PureStructs

userId :: Config.Config -> PureStructs.ChatID -> Query
userId (Config.VKBot _) usid = "\'" <> "vk" <> (fromString . show) usid <> "\'"
userId (Config.TBot _) usid = "\'" <> "tel" <> (fromString . show) usid <> "\'"

userIdText :: Config.Config -> PureStructs.ChatID -> T.Text
userIdText (Config.VKBot _) usid = "vk" <> (T.pack . show) usid
userIdText (Config.TBot _) usid = "tel" <> (T.pack . show) usid

findUserQuery :: Config.Config -> PureStructs.ChatID -> Query
findUserQuery bType usid =
  "SELECT repeats FROM UserRepeats WHERE userId = "
    <> userId bType usid

addUserQuery :: Query
addUserQuery = "INSERT INTO UserRepeats (userId, repeats) VALUES (?,?) returning userId, repeats"

updateUserQuery :: Query
updateUserQuery = "UPDATE UserRepeats SET repeats = ? WHERE userId = ? returning userId, repeats"
