module HEP.Automation.Model.Client.Config where

import Control.Monad.Identity
import Control.Applicative
import Text.Parsec
import HEP.Parser.Config

data ModelConfiguration = ModelConfiguration { 
  modelconf_modelserverurl :: String
} deriving (Show)

modelClientConfigParser :: ParsecT String () Identity ModelConfiguration 
modelClientConfigParser =
  oneGroupFieldInput "model" $
    ModelConfiguration <$> (oneFieldInput "server")


