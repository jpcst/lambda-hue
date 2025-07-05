{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bridge
import Network
import Payloads
import Math
import Types

import System.Environment (getArgs)
import Utils (runCommand)

import System.Directory (doesFileExist)
import Network.HTTP.Client (Manager)
import Data.List (isInfixOf, (\\))
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (void)
import System.Info (os)
import System.Process (callCommand)

clearScreen :: IO ()
clearScreen = callCommand $ if os == "mingw32" then "cls" else "clear"

-- Read or create a file
readOrCreateFile :: FilePath -> IO String
readOrCreateFile filePath = do
  fileExists <- doesFileExist filePath
  if fileExists then readFile filePath else writeFile filePath "" >> return ""

main :: IO ()
main = do
  let homeDirPath = "/Users/jpcst/bin/"
      ipFilePath = homeDirPath ++ "ip.txt"
      keyFilePath = homeDirPath ++ "key.txt"

  -- Read or create ip.txt file
  ipFileContent <- readOrCreateFile ipFilePath
  ip <- if ipFileContent == "" -- File does not exist or is empty
          then do
            bridge <- getMainBridgeIp -- Gets the ip from the API
            case bridge of
              Left err  -> putStrLn err >> return "" -- Some error, return nothing
              Right ip' -> writeFile ipFilePath ip' >> return ip' -- IP was found, save it to the file
          else return ipFileContent
  putStrLn $ "Found IP: " ++ ip ++ " at " ++ ipFilePath

  -- Read or create key.txt file
  keyFileContent <- readOrCreateFile keyFilePath
  key <- if keyFileContent == "" -- file does not exist or is empty
           then genKey ip keyFilePath -- user must press the Hue Bridge link button to generate a key and continue
           else return keyFileContent -- key was generated or already existed
  putStrLn $ "Found Key: " ++ key ++ " at " ++ keyFilePath

  args <- getArgs
  manager <- getInsecureManager -- HTTP request SSL disabled

  if not (null args)
    then do
      Right body <- sendGetRequest manager key (apiHueUrl ip "/light")
      let ids    = extractIdsFromBody    body
          states = extractStatesFromBody body
          bri    = extractBriFromBody    body
      runCommand manager key ip args ids states bri
    else
      loop manager key ip

loop :: Manager -> String -> String -> IO ()
loop manager key ip = do
  let hueUrl = apiHueUrl ip "/light" -- https://<ip>/clip/v2/resource/ ++ light
  result <- sendGetRequest manager key hueUrl
  case result of
    Left err -> do
      putStrLn $ "GET failed: " ++ err
      if "HttpExceptionRequest" `isInfixOf` err || "InvalidUrlException" `isInfixOf` err
        then do
          putStrLn "Detected invalid Bridge IP. Rewriting ip.txt..."
          writeFile "ip.txt" ""
      else if "Invalid Hue key" `isInfixOf` err || "no lighting" `isInfixOf` err
        then do
          putStrLn "Detected invalid Hue key. Rewriting key.txt..."
          writeFile "key.txt" ""
      else
        putStrLn "Unknown error, no file rewritten."
      putStrLn "Restarting main..."
      main


    Right body -> do
      let ids    = extractIdsFromBody body
          names  = extractNamesFromBody body
          states = extractStatesFromBody body
          bri    = extractBriFromBody body
      -- print ids
      -- print names
      -- print states
      -- print bri

      let customIdxs = [4, 2, 5, 0, 3, 1]
      let maxNameLength = maximum (map length names)
      let maxBriLength = 3  -- Fixed width for the 'bri' column
      let maxStateLength = 5  -- Fixed width for the 'state' column ('True' or 'False')
      putStrLn " |--- NAME ---|- STATE -|- BRI -|"
      putStrLn " |            |         |       |"
      mapM_ (\idx ->
          let name = names !! idx
              state = show (states !! idx)
              --bri' = show (round (bri !! idx) :: Int)  -- rounding the brightness value to remove decimal
              bri' = if states !! idx
                     then show (round (bri !! idx) :: Int)
                     else "==="
              -- Pad the name to the max length, left-align it
              paddedName = name ++ replicate (maxNameLength - length name) ' '
              -- Adjust state padding, ensuring fixed width for the state column
              paddedState = replicate (maxStateLength - length state) ' ' ++ state  -- Right-align 'state' in a 5-character wide column
              -- Adjust bri padding, ensuring fixed width for the bri column
              paddedBri = replicate (maxBriLength - length bri') ' ' ++ bri' -- Right-align 'bri' in a 5-character wide column
          in putStrLn $ " | " ++ paddedName ++ "  |  " ++ paddedState ++ "  |  " ++ paddedBri ++ "  |  "
        ) customIdxs
      putStrLn " |            |         |       |"
      putStrLn " |---|λHue|---|--|0.2|--|--|?|--|"

      let lightsIdx     = getListIndex ids
          lightsOnList  = getTrueIndexes states
          lightsOffList = lightsIdx \\ lightsOnList -- difference of Sets operator (\\) e.g. l = [1,2,3] and l' = [2] .:. l \\ l' = [1,3]

      -- putStrLn $ "\n" ++ "light id, idx"
      -- print $ zip ids lightsIdx
      -- print lightsIdxs
      -- putStrLn $ "\n" ++ "idx on list"
      -- print lightsOnList
      -- putStrLn $ "\n" ++ "idx off list"
      -- print lightsOffList

      let toggleLight position                = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadLightsToggle $ not $ states !! position
          setLightBri tt briPerc position     = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadSetBri tt briPerc
          setToggleAndBri tt briPerc position = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadToggleAndBri tt (not $ states !! position) briPerc
          setLightXy tt (x,y) position        = void $ sendPutRequest manager
                                                                      key
                                                                      (apiLightUrl hueUrl (ids !! position)) $
                                                                      buildJsonPayloadSetXy tt (x,y)

      userInput <- getLine
      clearScreen
      rawInput <- handleInput userInput
      case rawInput of
      -------------------------
      --   Hue Light Order   -- This is what the Hue app shows originally
      -------------------------
      --  [  CEILING 2  ]     0 idx
      --   , CEILING 4        1
      --   , DESK             2
      --   , CEILING 3        3
      --   , BED              4
      --   , CEILING 1  ]     5
      -------------------------
        S "d"  -> toggleLight 2
        S "b"  -> toggleLight 4
        S "c1" -> toggleLight 5
        S "c2" -> toggleLight 0
        S "c3" -> toggleLight 3
        S "c4" -> toggleLight 1
        -- S "c"  -> mapConcurrently_ toggleLight [5,0,3,1] -- Send the request to each CEILING light in parallel
        S "c"  -> mapM_ toggleLight [5,0,3,1]
        -- S "db" -> mapConcurrently_ toggleLight [2,4]     -- Send the request to DESK and BED lights in parallel
        S "db" -> mapM_ toggleLight [2,4]

        S s | s == "all" || s == "on"  -> mapM_ toggleLight lightsOffList
        S s | s == "off" || s == "nox" -> mapM_ toggleLight lightsOnList
        -- S s | s == "all" || s == "on"   -> mapConcurrently_ toggleLight lightsOffList
        -- S s | s == "nox" || s == "off" -> mapConcurrently_ toggleLight lightsOnList
        S "br"  -> mapM_ (setLightXy 0 (0.3   , 0.3))    (lightsOnList \\ [4]) -- I need to manually remove the BED light because the white LEDS are broken
        S "am"  -> mapM_ (setLightXy 0 (0.5203, 0.4141)) lightsOnList
        S "red" -> mapM_ (setLightXy 0 $ convertRgbToXy (256, 0, 0)) lightsOnList
        S "green" -> mapM_ (setLightXy 0 $ convertRgbToXy (0, 256, 0)) lightsOnList
        S "blue" -> mapM_ (setLightXy 0 $ convertRgbToXy (0, 0, 256)) lightsOnList
        -- S "br" -> mapM_ (setLightXy 0 (0.3   , 0.3))    lightsOnList
        -- S "am" -> mapM_ (setLightXy 0 (0.5435, 0.430)) lightsOnList

        I briPerc  -> mapConcurrently_ (setLightBri 0 briPerc) lightsOnList

        SI "d" briPerc
          | states !! 2 -> setLightBri     0 briPerc 2
          | otherwise   -> setToggleAndBri 0 briPerc 2
        SI "b" briPerc
          | states !! 4 -> setLightBri     0 briPerc 4
          | otherwise   -> setToggleAndBri 0 briPerc 4

        SI "c" briPerc ->
          mapConcurrently_ (\i ->
            if states !! i
              then setLightBri     0 briPerc i
              else setToggleAndBri 0 briPerc i
          ) [5,0,3,1]
        SI "db" briPerc ->
          mapConcurrently_ (\i ->
            if states !! i
              then setLightBri     0 briPerc i
              else setToggleAndBri 0 briPerc i
          ) [2,4]
         -- SI "b"  briPerc -> setLightBri 0 briPerc 4
        SI "c1" briPerc -> setLightBri 0 briPerc 5
        SI "c2" briPerc -> setLightBri 0 briPerc 0
        SI "c3" briPerc -> setLightBri 0 briPerc 3
        SI "c4" briPerc -> setLightBri 0 briPerc 1
        -- SI "c"  briPerc -> mapConcurrently_ (setLightBri 0 briPerc) [5,0,3,1]
        -- SI "db" briPerc -> mapConcurrently_ (setLightBri 0 briPerc) [2,4]
        SS "d" "br" -> setLightXy 0 (0.3   , 0.3)    2
        SS "d" "am" -> setLightXy 0 (0.5203, 0.4141) 2
        SS "c" "br" -> mapM_ (setLightXy 0 (0.3   , 0.3))    [5,0,3,1]
        SS "c" "am" -> mapM_ (setLightXy 0 (0.5203, 0.4141)) [5,0,3,1]
        SS "c" "."  -> mapM_ (setToggleAndBri 0 100) [5,0,3,1]

        III r g b -> mapM_ (setLightXy 0 $ convertRgbToXy (r,g,b)) lightsOnList
        _ -> return ()

      loop manager key ip
