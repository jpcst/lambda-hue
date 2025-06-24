module Utils (runCommand) where

import Bridge
import Payloads
import Types()
import Network
import Math

import Network.HTTP.Client (Manager)
import Control.Monad (void)
import Text.Read (readMaybe)

toggleMap :: [(String, [Int])]
toggleMap =
  [ ("d",    [2])
  , ("b",    [4])
  , ("c1",   [5])
  , ("c2",   [0])
  , ("c3",   [3])
  , ("c4",   [1])
  , ("c",    [5,0,3,1])
  , ("all",  [0..5])
  ]

runCommand :: Manager -> String -> String -> [String] -> [String] -> [Bool] -> [Double] -> IO ()
runCommand manager key ip args ids states bri = case args of

  [name] -> case lookup name toggleMap of -- ["toggle", name]
    Just [idx] -> toggle idx
    Just idxs -> mapM_ toggle idxs
    Nothing   -> putStrLn $ "Unknown name: " ++ name

  ["bri", percStr] -> case readMaybe percStr :: Maybe Int of
    Just perc ->
      let onLights = getTrueIndexes states
      in case onLights of
           [idx] -> setBri perc idx
           idxs  -> mapM_ (setBri perc) idxs
    Nothing ->
      putStrLn "Usage:\n  lambda-hue <name>\n  lambda-hue bri <0–100>\n  lambda-hue <name> <0–100>"

  ["inc", incStr] -> case readMaybe incStr :: Maybe Int of
    Just inc ->
      let onLights = getTrueIndexes states
      in case onLights of
        [idx] -> setBri (round (bri !! idx) + inc) idx
        idxs -> mapM_ (\i -> setBri (round (bri !! i) + inc) i) idxs
    Nothing -> putStrLn "Usage: lambda-hue inc <int>"

  [name, percStr] -> case (lookup name toggleMap, readMaybe percStr :: Maybe Int) of
    (Just [idx], Just perc) -> setBri perc idx
    (Just idxs, Just perc) -> mapM_ (setBri perc) idxs
    (Nothing, _) -> putStrLn $ "Unknown name: " ++ name
    (_, Nothing) -> putStrLn "Brightness must be an integer (0–100)."

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