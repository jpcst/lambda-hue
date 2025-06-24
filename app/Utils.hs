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

-- runCommand :: Manager -> String -> String -> [String] -> [String] -> [Bool] -> IO ()
-- runCommand manager key ip args ids states = case args of
--   ["toggle", name] -> case lookup name toggleMap of
--     Just idxs -> mapM_ toggle idxs
--     Nothing   -> putStrLn $ "Unknown name: " ++ name
--   _ -> putStrLn "Usage: lambda-hue toggle <desk|bed|c1|c2|c3|c4|c|all>"
--   where
--     hueUrl = apiHueUrl ip "/light"
--     toggle idx =
--       let currentState = states !! idx
--           lightId = ids !! idx
--       in void $
--          sendPutRequest manager key (apiLightUrl hueUrl lightId) $
--          buildJsonPayloadLightsToggleWithTt 0 (not currentState)

runCommand :: Manager -> String -> String -> [String] -> [String] -> [Bool] -> IO ()
runCommand manager key ip args ids states = case args of

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
         -- sendPutRequest manager key (apiLightUrl hueUrl lightId) $
         -- buildJsonPayloadSetBri 0 perc
         sendPutRequest manager key (apiLightUrl hueUrl lightId) payload