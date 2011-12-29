{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Model.Client.Job where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Aeson.Types
import Data.Aeson.Encode as E
import Data.Aeson.Parser
import qualified Data.Attoparsec as A

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator

import System.Directory 
import System.FilePath
import Unsafe.Coerce

import HEP.Automation.Model.Client.Config
import HEP.Automation.Model.Type
import Data.UUID
import Data.UUID.V5
import qualified Data.ByteString as B
import Data.Time.Clock

type Url = String 

nextUUID :: ModelClientConfiguration -> IO UUID
nextUUID mc = do 
  let c = modelClientURL mc 
  t <- getCurrentTime 
  return . generateNamed namespaceURL . B.unpack . SC.pack $ c ++ "/" ++ show t 

startCreate :: ModelClientConfiguration -> String -> IO () 
startCreate mc name = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = modelServerURL mc 
  uuid <- nextUUID mc
  let info = ModelInfo { model_uuid = uuid , model_name = name } 
  response <- modelToServer url ("uploadmodel") methodPost info
  putStrLn $ show response 


startGet :: ModelClientConfiguration -> String -> IO () 
startGet mc idee = do 
  putStrLn $"get " ++ idee
  let url = modelServerURL mc 
  r <- jsonFromServer url ("model" </> idee) methodGet
  putStrLn $ show r 


startPut :: ModelClientConfiguration 
         -> String  -- ^ model idee
         -> String  -- ^ model name 
         -> IO () 
startPut mc idee name = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = modelServerURL mc 
      info = case fromString idee of 
               Nothing -> error "strange in startPut" 
               Just idee' -> ModelInfo { model_uuid = idee', model_name = name }
  response <- modelToServer url ("model" </> idee) methodPut info
  putStrLn $ show response 


startDelete :: ModelClientConfiguration -> String -> IO () 
startDelete mc idee = do 
  putStrLn "job started"
  let url = modelServerURL mc 
  r <- jsonFromServer url ("model" </> idee) methodDelete
  putStrLn $ show r 


startGetList :: ModelClientConfiguration -> IO () 
startGetList mc = do 
  putStrLn "getlist: "
  let url = modelServerURL mc 
  r <- jsonFromServer url ("listmodel") methodGet
  putStrLn $ show r 


jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestjson = request { 
          method = mthd,
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] } 
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

modelToServer :: Url -> String -> Method -> ModelInfo -> IO (Either String (Result Value))
modelToServer url api mthd mi = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    r <- httpLbs requestjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode r)) 

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) $ A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 
