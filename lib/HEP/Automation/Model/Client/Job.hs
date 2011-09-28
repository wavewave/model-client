module HEP.Automation.Model.Client.Job where

import HEP.Automation.Model.Client.Config

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Types
import Data.Aeson.Encode
import Data.Aeson.Parser

-- import Data.Attoparsec


import Data.Data

import System.FilePath

type Url = String 

startCreate :: ModelConfiguration -> FilePath -> IO () 
startCreate mc fp = do 
  putStrLn "job started"

startGet :: ModelConfiguration -> String -> IO () 
startGet mc name = do 
  putStrLn $"get " ++ name
  let url = modelconf_modelserverurl mc 
  r <- getJsonFromServer url ("model" </> name) 
  putStrLn $ show r 


startPut :: ModelConfiguration -> FilePath -> IO () 
startPut mc fp = do 
  putStrLn "job started"

startDelete :: ModelConfiguration -> String -> IO () 
startDelete mc name = do 
  putStrLn "job started"


getJsonFromServer :: Url -> String -> IO (Maybe Value)
getJsonFromServer url api = do 
  withManager $ \manager -> do
    requestget <- parseUrl (url </> api)
    let requestgetjson = requestget { 
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        } 
    r <- httpLbs requestgetjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return Nothing

parseJson :: (Data a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = parse json bs
  in case resultjson of 
       Success rjson -> Right (fromJSON rjson)
       _            -> Left "error" 
