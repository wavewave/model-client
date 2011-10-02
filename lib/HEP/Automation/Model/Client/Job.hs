{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Model.Client.Job where

import HEP.Automation.Model.Client.Config
import HEP.Automation.Model.Type

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Types
import Data.Aeson.Encode
import Data.Aeson.Parser
import Data.Aeson.Generic as G

import qualified Data.Attoparsec as A

import System.FilePath

type Url = String 

testmodel1 = ModelInfo { 
  model_name = "testmodel1", 
  model_baseurl = "http://susy.physics.lsa.umich.edu/testmodel1", 
  model_feynrules = "testmodel1.fr", 
  model_ufo = "testmodel1_UFO"
}

testmodel2 = ModelInfo { 
  model_name = "testmodel2", 
  model_baseurl = "http://susy.physics.lsa.umich.edu/testmodel2", 
  model_feynrules = "newmodel2.fr", 
  model_ufo = "testmodel2_UFO"
}

startCreate :: ModelConfiguration -> FilePath -> IO () 
startCreate mc _fp = do 
  putStrLn "job started"
  let url = modelconf_modelserverurl mc 
  r <- modelToServer url ("uploadmodel") methodPost testmodel1 
  putStrLn $ show r 



startGet :: ModelConfiguration -> String -> IO () 
startGet mc name = do 
  putStrLn $"get " ++ name
  let url = modelconf_modelserverurl mc 
  r <- jsonFromServer url ("model" </> name) methodGet
  putStrLn $ show r 


startPut :: ModelConfiguration -> String -> IO () 
startPut mc name = do 
  putStrLn "job started"
  let url = modelconf_modelserverurl mc 
  r <- modelToServer url ("model" </> name) methodPut (testmodel2 { model_name = name }) 
  putStrLn $ show r 





startDelete :: ModelConfiguration -> String -> IO () 
startDelete mc name = do 
  putStrLn "job started"
  let url = modelconf_modelserverurl mc 
  r <- jsonFromServer url ("model" </> name) methodDelete
  putStrLn $ show r 


startGetList :: ModelConfiguration -> IO () 
startGetList mc = do 
  putStrLn "getlist: "
  let url = modelconf_modelserverurl mc 
  r <- jsonFromServer url ("listmodel") methodGet
  putStrLn $ show r 


jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestjson = request { 
          method = mthd,
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        } 
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

modelToServer :: Url -> String -> Method -> ModelInfo -> IO (Either String (Result Value))
modelToServer url api mthd mi = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let mijson = encode (G.toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]  
          , requestBody = myrequestbody
          } 

    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 



parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "error" 
