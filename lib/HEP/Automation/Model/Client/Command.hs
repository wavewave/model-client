module HEP.Automation.Model.Client.Command where

import HEP.Automation.Model.Client.Type
import HEP.Automation.Model.Client.Job

commandLineProcess :: Model_client -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
