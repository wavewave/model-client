module HEP.Automation.Model.Client.Command where

import HEP.Automation.Model.Client.Type
import HEP.Automation.Model.Client.Job
import HEP.Automation.Model.Client.Config
import HEP.Util.Parsing 

commandLineProcess :: Model_client -> IO ()
commandLineProcess (Create cfg mn) = do 
  putStrLn "create called"
  c <- readConfig cfg modelClientConfigParser 
  startCreate c mn
commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  c <- readConfig cfg modelClientConfigParser 
  startGet c n
commandLineProcess (Put cfg n mn) = do 
  putStrLn "put called"
  c <- readConfig cfg modelClientConfigParser 
  startPut c n mn
commandLineProcess (Delete cfg n) = do 
  putStrLn "delete called"
  c <- readConfig cfg modelClientConfigParser 
  startDelete c n
commandLineProcess (GetList cfg) = do 
  putStrLn "getlist called"
  c <- readConfig cfg modelClientConfigParser 
  startGetList c 