{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import GHC.Generics
import Control.Applicative
import System.Environment

data Currency = Currency { 
  base :: String,
  date :: String,
  rates :: Object } deriving (Show, Generic)

instance FromJSON Currency where
  parseJSON = withObject "currency" $ \obj -> do
    base <- obj .: "base"
    date <- obj .: "date"
    rates <- obj .: "rates"
    return Currency{..}

jsonURL :: String
jsonURL = "https://api.fixer.io/latest"

argsCheck :: String -> [String] -> String
argsCheck url args = 
  case args of
    [] -> url
    _ -> url ++ "?symbols=" ++ (foldr (++) [] (fmap (\el -> el++",") args))

getJSON :: IO BSL.ByteString
getJSON = 
  do
    args <- getArgs
    simpleHttp $ argsCheck jsonURL args

main :: IO ()
main = do
  jsonOut <- (eitherDecode' <$> getJSON) :: IO (Either String Currency)
  case jsonOut of
    Right err -> print err
    Left ps -> print ps