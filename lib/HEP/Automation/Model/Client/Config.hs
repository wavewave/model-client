{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.Model.Client.Config where

-- import Control.Monad.Identity
import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types
-- import Text.Parsec
-- import HEP.Parser.Config

data ModelClientConfiguration = ModelClientConfiguration { 
  modelServerURL :: String,
  modelClientURL :: String
} deriving (Show)

getModelClientConfiguration :: Config -> IO (Maybe ModelClientConfiguration)
getModelClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  (ModelClientConfiguration  <$> s <*> c )
{-
modelClientConfigParser :: ParsecT String () Identity ModelConfiguration 
modelClientConfigParser =
  oneGroupFieldInput "model" $
    ModelConfiguration <$> (oneFieldInput "server")
-}

