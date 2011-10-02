{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.Model.Client.Type where 

import System.FilePath
import System.Console.CmdArgs hiding (name)

data Model_client = Create { config :: FilePath, content :: FilePath }
                  | Get    { config :: FilePath, name :: String } 
                  | Put    { config :: FilePath, content :: FilePath } 
                  | Delete { config :: FilePath, name :: String } 
                  | GetList { config :: FilePath } 
              deriving (Show,Data,Typeable)

create :: Model_client
create = Create { config = "test.conf"
                , content = "" &= typ "CONTENT" &= argPos 0
                }

get :: Model_client 
get = Get { config = "test.conf" 
          , name = "" &= typ "NAME" &= argPos 0 
          } 

put :: Model_client 
put = Put { config = "test.conf"
          , content = "" &= typ "CONTENT" &= argPos 0 
          }

delete :: Model_client 
delete = Delete { config = "test.conf"
                , name = "" &= typ "NAME" &= argPos 0 
                }

getlist :: Model_client 
getlist = GetList { config = "test.conf" } 

mode = modes [ create, get, put, delete, getlist ]


