{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Model.Client.Job where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Aeson.Types
import Data.Aeson.Encode
import Data.Aeson.Parser
import Data.Aeson.Generic as G
import qualified Data.Attoparsec as A

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator

import System.Directory 
import System.FilePath
import Unsafe.Coerce

import HEP.Automation.Model.Client.Config
import HEP.Automation.Model.Type
import HEP.Util.GHC.Plugins

type Url = String 

startCreate :: ModelConfiguration -> String -> IO () 
startCreate mc mname = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = modelconf_modelserverurl mc 
  let fullmname = mname
  r <- pluginCompile cwd fullmname "model"
  case r of 
    Left err -> putStrLn err 
    Right value -> do 
     let model = unsafeCoerce value :: ModelInfo
     putStrLn $ show model 
     response <- modelToServer url ("uploadmodel") methodPost model
     putStrLn $ show response 

startGet :: ModelConfiguration -> String -> IO () 
startGet mc name = do 
  putStrLn $"get " ++ name
  let url = modelconf_modelserverurl mc 
  r <- jsonFromServer url ("model" </> name) methodGet
  putStrLn $ show r 


startPut :: ModelConfiguration 
         -> String  -- ^ model name
         -> String  -- ^ module name 
         -> IO () 
startPut mc name modname = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = modelconf_modelserverurl mc 
  r <- pluginCompile cwd modname "model"
  case r of 
    Left err -> putStrLn err 
    Right value -> do 
     let model = unsafeCoerce value :: ModelInfo
     putStrLn $ show model 
     response <- modelToServer url ("model" </> name) methodPut model
     putStrLn $ show response 

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
