{-# LANGUAGE OverloadedStrings #-}
module Payloads where

import qualified Data.Aeson as Aeson
import           Data.Aeson ( (.=), object )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Vector as V

-- JSON to generate a new key
buildJsonPayloadGenKey :: Aeson.Value
buildJsonPayloadGenKey = object
  [ "devicetype"        .= ("lambda-hue#0.1" :: String)
  , "generateclientkey" .= True
  ]

-- JSON to toggle lights
buildJsonPayloadLightsToggle :: Bool -> Aeson.Value
buildJsonPayloadLightsToggle b = object
  [ "on"       .= object [ "on" .= b ] -- True to turn ON, False to turn OFF
  , "dynamics" .= object [ "duration" .= (0 :: Int) ]
  ]

-- JSON to toggle lights with transitionTime
buildJsonPayloadLightsToggleWithTt :: Int -> Bool -> Aeson.Value
buildJsonPayloadLightsToggleWithTt transitionTime b = object
  [ "on"       .= object [ "on" .= b ] -- True to turn ON, False to turn OFF
  , "dynamics" .= object [ "duration" .= transitionTime ]
  ]

-- JSON to set the brightness
-- The light does not need to be ON to send the command, if the light if OFF when sent, when toggled ON it will be at the previously defined BRI
buildJsonPayloadSetBri :: Int -> Int -> Aeson.Value
buildJsonPayloadSetBri transitionTime bri = object
  [ "dimming"       .= object [ "brightness" .= bri ]
  , "dynamics" .= object [ "duration" .= transitionTime ] -- Default transition time is 400 (ms). Set to 0 for max speed
  ]

-- JSON to toggle lights at a given brightness
buildJsonPayloadToggleAndBri :: Int -> Bool -> Int -> Aeson.Value
buildJsonPayloadToggleAndBri transitionTime b bri = object
  [ "on"       .= object [ "on" .= b ] -- True to turn ON, False to turn OFF
  , "dimming"  .= object [ "brightness" .= bri]
  , "dynamics" .= object [ "duration" .= transitionTime ]
  ]

-- JSON to set the color (x,y) 
-- CIE 1931 for reference
-- convertRgbToXy function available on Math.hs
buildJsonPayloadSetXy :: Int -> (Double, Double) -> Aeson.Value
buildJsonPayloadSetXy transitionTime (x,y) = object -- Hue API default transition time is 400 (ms)
  [ "color" .= object
      [ "xy"       .= object [ "x" .= x, "y" .= y ] -- True to turn ON, False to turn OFF
      , "dynamics" .= object [ "duration" .= transitionTime ] -- Default transition time is 400 (ms). Set to 0 for max speed
      ]
  ]

-- Gets the lights IDs as a list, will be the same order as the default Hue app
extractIdsFromBody :: L8.ByteString -> [String]
extractIdsFromBody body =
  case Aeson.decode body :: Maybe Aeson.Object of
    Just obj ->
      case KM.lookup "data" obj of
        Just (Aeson.Array arr) ->
          [ T.unpack idText
          | Aeson.Object item <- V.toList arr
          , Just (Aeson.String idText) <- [KM.lookup "id" item]
          ]
        _ -> []
    Nothing -> []

-- Gets the names as a list
extractNamesFromBody :: L8.ByteString -> [String]
extractNamesFromBody body =
  case Aeson.decode body :: Maybe Aeson.Object of
    Just obj ->
      case KM.lookup "data" obj of
        Just (Aeson.Array arr) ->
          [ T.unpack nameText
          | Aeson.Object item <- V.toList arr
          , Just (Aeson.Object metadata) <- [KM.lookup "metadata" item]
          , Just (Aeson.String nameText) <- [KM.lookup "name" metadata]
          ]
        _ -> []
    Nothing -> []

-- Gets the states as a list (true -> on, false -> off)
extractStatesFromBody :: L8.ByteString -> [Bool]
extractStatesFromBody body =
  case Aeson.decode body :: Maybe Aeson.Object of
    Just obj ->
      case KM.lookup "data" obj of
        Just (Aeson.Array arr) ->
          [ b
          | Aeson.Object item <- V.toList arr
          , Just (Aeson.Object onObj) <- [KM.lookup "on" item]
          , Just (Aeson.Bool b) <- [KM.lookup "on" onObj]
          ]
        _ -> []
    Nothing -> []

-- Gets the brightness as a list (range from 0.0 to 100.0)
extractBriFromBody :: L8.ByteString -> [Double]
extractBriFromBody body =
  case Aeson.decode body :: Maybe Aeson.Object of
    Just obj ->
      case KM.lookup "data" obj of
        Just (Aeson.Array arr) ->
          [ realToFrac n
          | Aeson.Object item <- V.toList arr
          , Just (Aeson.Object dimming) <- [KM.lookup "dimming" item]
          , Just (Aeson.Number n) <- [KM.lookup "brightness" dimming]
          ]
        _ -> []
    Nothing -> []