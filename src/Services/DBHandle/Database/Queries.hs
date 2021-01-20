module Services.DBHandle.Database.Queries where

import qualified Data.Text as T 
import qualified Config.Config as Config 
import Database.PostgreSQL.Simple ( Query ) 
import Data.String ( IsString(fromString) ) 

userId :: Config.Config -> Int -> Query
userId (Config.VKBot _) usid = "\'" <> "VK" <> (fromString . show) usid <> "\'" 
userId (Config.TBot _) usid = "\'" <> "Tel" <> (fromString . show) usid <> "\'" 

userIdText :: Config.Config -> Int -> T.Text
userIdText (Config.VKBot _) usid = "VK" <> (T.pack . show) usid 
userIdText (Config.TBot _) usid = "Tel" <> (T.pack . show) usid 

findUserQuery :: Config.Config -> Int -> Query
findUserQuery bType usid = "SELECT repeats FROM UserRepeats WHERE userId = " 
    <> userId bType usid

addUserQuery :: Query  
addUserQuery = "INSERT INTO UserRepeats (userId, repeats) VALUES (?,?) returning userId, repeats" 
    

updateUserQuery :: Query
updateUserQuery = "UPDATE UserRepeats SET repeats = ? WHERE userId = ? returning userId, repeats" 