module HEP.Automation.Model.Client.Command where

import HEP.Automation.Model.Client.Type
import HEP.Automation.Model.Client.Job
import HEP.Automation.Model.Client.Config
import HEP.Util.Parsing 

commandLineProcess :: Model_client -> IO ()
commandLineProcess (Create cfg cnt) = do 
  putStrLn "create called"
  c <- readConfig cfg modelClientConfigParser 
  startCreate c cnt
commandLineProcess (Get cfg name) = do 
  putStrLn "get called"
  c <- readConfig cfg modelClientConfigParser 
  startGet c name
commandLineProcess (Put cfg ctt) = do 
  putStrLn "put called"
  c <- readConfig cfg modelClientConfigParser 
  startPut c ctt
commandLineProcess (Delete cfg name) = do 
  putStrLn "delete called"
  c <- readConfig cfg modelClientConfigParser 
  startDelete c name
