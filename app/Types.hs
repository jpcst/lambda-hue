{-# LANGUAGE OverloadedStrings #-}

module Types where
import qualified Data.Aeson as Aeson

data InputValue
  = S   String
  | I   Int
  | SI  String Int
  | SS  String String
  | III Int Int Int
  deriving (Show)

data HueBridge = HueBridge
  { id :: String
  , internalipaddress :: String
  , port :: Integer
  } deriving (Show)

instance Aeson.FromJSON HueBridge where
  parseJSON = Aeson.withObject "HueBridge" $ \v ->
    HueBridge <$> v Aeson..: "id"
              <*> v Aeson..: "internalipaddress"
              <*> v Aeson..: "port"