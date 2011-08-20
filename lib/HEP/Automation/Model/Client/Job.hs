module HEP.Automation.Model.Client.Job where

import HEP.Automation.Model.Client.Config

startCreate :: ModelConfiguration -> FilePath -> IO () 
startCreate mc fp = do 
  putStrLn "job started"

startGet :: ModelConfiguration -> String -> IO () 
startGet mc name = do 
  putStrLn "job started"

startPut :: ModelConfiguration -> FilePath -> IO () 
startPut mc fp = do 
  putStrLn "job started"

startDelete :: ModelConfiguration -> String -> IO () 
startDelete mc name = do 
  putStrLn "job started"
