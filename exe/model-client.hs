module Main where

import System.Console.CmdArgs

import HEP.Automation.Model.Client.ProgType
import HEP.Automation.Model.Client.Command

main :: IO () 
main = do 
  putStrLn "model-client"
  param <- cmdArgs mode
  commandLineProcess param