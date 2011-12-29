module HEP.Automation.Model.Client.Command where

import HEP.Automation.Model.Client.ProgType
import HEP.Automation.Model.Client.Job
import HEP.Automation.Model.Client.Config
import Data.Configurator

commandLineProcess :: Model_client -> IO ()
commandLineProcess (Create cfg mn) = do 
  putStrLn "create called"
  mc <- getModelClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startCreate mn) mc
commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  mc <- getModelClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startGet n) mc
commandLineProcess (Put cfg n mn) = do 
  putStrLn "put called"
  mc <- getModelClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\c-> startPut c n mn) mc
commandLineProcess (Delete cfg n) = do 
  putStrLn "delete called"
  mc <- getModelClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startDelete n) mc
commandLineProcess (GetList cfg) = do 
  putStrLn "getlist called"
  mc <- getModelClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") startGetList mc
