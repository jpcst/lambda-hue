module Utils (runCommand) where

import Bridge
import Payloads
import Types()
import Network
import Math (convertRgbToXy, getTrueIndexes)

import Network.HTTP.Client (Manager)
import Control.Monad (void)
import Text.Read (readMaybe)
import Data.List ( (\\) )

toggleMapLight :: [ (String, [Int]) ]
toggleMapLight =
  [ ("d",   [2])
  , ("b",   [4])
  , ("c1",  [5])
  , ("c2",  [0])
  , ("c3",  [3])
  , ("c4",  [1])
  , ("c",   [5,0,3,1])
  , ("all", [0..5])
  ]

toggleMapColors :: [ (String, (Double, Double)) ]
toggleMapColors =
  [ ("br",     (0.3333, 0.3333))
  , ("white",  (0.3333, 0.3333))
  , ("am",     (0.5203, 0.4141))
  , ("red",    convertRgbToXy (256, 0, 0))
  , ("green",  convertRgbToXy (0, 256, 0))
  , ("blue",   convertRgbToXy (0, 0, 256))
  , ("indigo", convertRgbToXy (75, 0, 130))
  ]

runCommand :: Manager -> String -> String -> [String] -> [String] -> [Bool] -> [Double] -> IO ()
runCommand manager key ip args ids states bri = case args of

  --[name] -> case lookup name toggleMapLight of -- ["toggle", name]
  --  Just [idx] -> toggle idx
  --  Just idxs  -> mapM_ toggle idxs
  --  Nothing    -> putStrLn $ "Unknown name: " ++ name
  [name] ->
    if name == "nox"
      then do
        let onLights = getTrueIndexes states
        case onLights of
          [idx] -> toggle idx
          idxs  -> mapM_ toggle idxs
    else case lookup name toggleMapLight of
      Just [idx] -> toggle idx
      Just idxs  -> mapM_ toggle idxs
      Nothing    -> putStrLn $ "Unknow name: " ++ name


  ["rgb", rStr, gStr, bStr] ->
    case (readMaybe rStr :: Maybe Int, readMaybe gStr :: Maybe Int, readMaybe bStr :: Maybe Int) of
      (Just r, Just g, Just b) ->
        let (x, y)   = convertRgbToXy (r, g, b)
            onLights = getTrueIndexes states
        in case onLights of
          [idx] -> setXy (x, y) idx
          idxs  -> mapM_ (setXy (x, y)) idxs
      _ -> putStrLn "Usage: lambda-hue rgb <r> <g> <b> (0-255)"

  ["bri", percStr] -> case readMaybe percStr :: Maybe Int of
    Just perc ->
      let onLights = getTrueIndexes states
      in case onLights of
           [idx] -> setBri perc idx
           idxs  -> mapM_ (setBri perc) idxs
    Nothing -> putStrLn "Usage:\n  lambda-hue <name>\n  lambda-hue bri <0–100>\n  lambda-hue <name> <0–100>"

  ["inc", incStr] -> case readMaybe incStr :: Maybe Int of
    Just inc ->
      let onLights  = getTrueIndexes states
      in case onLights of
        [idx] -> setBri (round (bri !! idx) + inc) idx
        idxs  -> mapM_ (\i -> setBri (round (bri !! i) + inc) i) idxs
    Nothing -> putStrLn "Usage: lambda-hue inc <int>"

  ["clr", colorStr] -> case lookup colorStr toggleMapColors of
    Just (x,y) ->
      let onLights         = getTrueIndexes states
          onLightsFiltered = if colorStr == "br" then onLights \\ [4] else onLights -- White LEDs are broken for Bed, so I need to remove it from changing
      in case onLightsFiltered of
        [idx] -> setXy (x,y) idx
        idxs  -> mapM_ (setXy (x,y)) idxs
    Nothing -> putStrLn "Unknow color"

  [name, percStr] -> case (lookup name toggleMapLight, readMaybe percStr :: Maybe Int) of
    (Just [idx], Just perc) -> setBri perc idx
    (Just idxs, Just perc)  -> mapM_ (setBri perc) idxs
    (Nothing, _)            -> putStrLn $ "Unknown name: " ++ name
    (_, Nothing)            -> putStrLn "Brightness must be an integer (0–100)."
    _                       -> putStrLn "ダミーテキスト"

  _ -> putStrLn "Usage:\n  lambda-hue <name>\n  lambda-hue bri <0–100>\n  lambda-hue <name> <0–100>"

  where
    hueUrl = apiHueUrl ip "/light"

    toggle idx =
      let currentState = states !! idx
          lightId = ids !! idx
      in void $
        sendPutRequest manager key (apiLightUrl hueUrl lightId) $
        buildJsonPayloadLightsToggleWithTt 0 (not currentState)

    setBri perc idx =
      let lightId = ids !! idx
          isOn    = states !! idx
          payload = if isOn
            then buildJsonPayloadSetBri 0 perc
            else buildJsonPayloadToggleAndBri 0 True perc
      in void $
        sendPutRequest manager key (apiLightUrl hueUrl lightId) payload

    setXy xy idx =
      let lightId = ids !! idx
          -- isOn    = states !! idx
          payload = buildJsonPayloadSetXy 0 xy
      in void $
        sendPutRequest manager key (apiLightUrl hueUrl lightId) payload
