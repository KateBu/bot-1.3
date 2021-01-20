module Services.DBHandle.DBHandle where

import Control.Monad.Reader ( ReaderT(runReaderT) )
import qualified Environment.Environment as Env
import qualified Services.DBHandle.Database.Database as DB

data Handle m = Handle 
    {
        findUser :: Int -> m (Maybe Int) ,
        addUSer :: Int -> Int -> m ()  ,
        updateUSer :: Int -> Int -> m ()  
    }

new :: Env.Environment IO -> IO (Handle IO)
new env = do 
    config <- runReaderT Env.eConfig env   
    logger <- runReaderT Env.eGetLogger env   
    pure $ Handle 
        {
            findUser = DB.find config logger,
            addUSer = DB.add config logger, 
            updateUSer = DB.update config logger
        }