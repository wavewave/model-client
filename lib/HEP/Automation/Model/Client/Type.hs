{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.Model.Client.Type where 

import System.Console.CmdArgs

data Model_client = Test 
              deriving (Show,Data,Typeable)

test :: Model_client
test = Test 

mode = modes [test]

