{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Model.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data ModelClientConfiguration = ModelClientConfiguration { 
  modelServerURL :: String,
  modelClientURL :: String
} deriving (Show)

getModelClientConfiguration :: Config -> IO (Maybe ModelClientConfiguration)
getModelClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  (ModelClientConfiguration  <$> s <*> c )

