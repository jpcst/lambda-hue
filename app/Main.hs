{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import LambdaHue.Utils

import System.Directory (doesFileExist)
import Network.HTTP.Client
  ( Request(..), RequestBody(..), method, requestBody
  , parseRequest, Manager, httpLbs, responseBody, Response (responseBody)
  )
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.TLS ( ClientParams(..), defaultParamsClient, Shared(..), ValidationCache(..)  )
import Data.X509.Validation (ValidationCacheResult(..)) -- <- ValidationCachePass lives here
import Network.Connection (TLSSettings(..))
import Data.Default.Class (def)
import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.ByteString.Lazy.Search as BLS
import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object)
import Network.HTTP.Types.Method (methodPut)
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Control.Concurrent.Async (mapConcurrently_)
import Data.Char (isAlpha, isDigit)
import Text.Read (readMaybe)
-- import qualified Data.Map as M
import Data.List ((\\))
import Control.Exception (try, SomeException)
import Control.Monad (void)
import System.Info (os)
import System.Process (callCommand)
-- import qualified Network.HTTP.Client as L8

clearScreen :: IO ()
clearScreen = callCommand $ if os == "mingw32" then "cls" else "clear"

-- Represents the Hue Bridge Discovery IP JSON
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

-- Reads or creates the file
readOrCreateFile :: FilePath -> IO String
readOrCreateFile filePath = do
  fileExists <- doesFileExist filePath
  if fileExists then readFile filePath else writeFile filePath "" >> return ""

buildJsonPayloadLightsToggle :: Bool -> Aeson.Value
buildJsonPayloadLightsToggle b = object
  [ "on"       .= object [ "on" .= b ] -- True to turn ON, False to turn OFF
  , "dynamics" .= object [ "duration" .= (0 :: Int) ]
  ]

-- The light does not need to be ON to send the command, if the light if OFF when sent, when toggled ON it will be at the previously defined BRI
buildJsonPayloadSetBri :: Int -> Int -> Aeson.Value
buildJsonPayloadSetBri transitionTime bri = object
  [ "dimming"       .= object [ "brightness" .= bri ]
  , "dynamics" .= object [ "duration" .= transitionTime ] -- Default transition time is 400 (ms). Set to 0 for max speed
  ]

buildJsonPayloadToggleAndBri :: Int -> Bool -> Int -> Aeson.Value
buildJsonPayloadToggleAndBri transitionTime b bri = object
  [ "on"       .= object [ "on" .= b ] -- True to turn ON, False to turn OFF
  , "dimming"  .= object [ "brightness" .= bri]
  , "dynamics" .= object [ "duration" .= transitionTime ]
  ]

buildJsonPayloadSetXy :: Int -> (Double,Double) -> Aeson.Value
buildJsonPayloadSetXy transitionTime (x,y) = object -- Hue API default transition time is 400 (ms)
  [ "color" .= object
      [ "xy"       .= object [ "x" .= x, "y" .= y ] -- True to turn ON, False to turn OFF
      , "dynamics" .= object [ "duration" .= transitionTime ] -- Default transition time is 400 (ms). Set to 0 for max speed
      ]
  ]

buildJsonPayloadGenKey :: Aeson.Value
buildJsonPayloadGenKey = object
  [ "devicetype"        .= ("lambda-hue#test01" :: String)
  , "generateclientkey" .= True
  ]

-- Disables SSL verification and creates a manager
getInsecureManager :: IO Manager
getInsecureManager = do
  let clientParams = (defaultParamsClient "" "")
        { clientSupported = def
        , clientShared = def
            { sharedValidationCache = ValidationCache
                (\_ _ _ -> return ValidationCachePass)
                (\_ _ _ -> return ())
            }
        }
  newTlsManagerWith $ mkManagerSettings (TLSSettings clientParams) Nothing

sendPutRequest :: Manager -> String -> String -> Aeson.Value -> IO L8.ByteString
sendPutRequest manager key url jsonPayload = do
  initialRequest <- parseRequest url
  let request = initialRequest
        { method         = methodPut
        , requestBody    = RequestBodyLBS $ Aeson.encode jsonPayload
        , requestHeaders = [("hue-application-key", BS.pack key)]
        }
  response <- httpLbs request manager
  -- putStrLn "End of PUT fn... PUT Response:"
  -- L8.putStrLn $ responseBody response
  return $ responseBody response

-- sendGetRequest :: Manager -> String -> String -> IO L8.ByteString
-- sendGetRequest manager key url = do
--   initialRequest <- parseRequest url
--   let request = initialRequest
--         { method = "GET"
--         , requestHeaders = [("hue-application-key", BS.pack key)]
--         }
--   response <- httpLbs request manager
--   putStrLn "End of GET fn... GET Response:"
--   let body = responseBody response
--   -- L8.putStrLn body
--   return body

-- sendGetRequest :: Manager -> String -> String -> IO (Either String L8.ByteString)
-- sendGetRequest manager key url = do
--   result <- try $ do
--     initialRequest <- parseRequest url
--     let request = initialRequest
--           { method = "GET"
--           , requestHeaders = [("hue-application-key", BS.pack key)]
--           }
--     response <- httpLbs request manager
--     putStrLn "End of GET fn... GET Response:"
--     return (responseBody response)
--   case result of
--     Left err   -> return (Left $ show (err :: SomeException))
--     Right body -> return (Right body)

sendGetRequest :: Manager -> String -> String -> IO (Either String L8.ByteString)
sendGetRequest manager key url = do
  result <- try $ do
    initialRequest <- parseRequest url
    let request = initialRequest
          { method = "GET"
          , requestHeaders = [("hue-application-key", BS.pack key)]
          }
    response <- httpLbs request manager
    -- putStrLn "End of GET fn... GET Response:"
    return $   responseBody response
  case result of
    Left err   -> return (Left $ show (err :: SomeException))
    Right body -> do
      let bodyStr = L8.unpack body
      if "<html>" `isInfixOf` bodyStr && "no lighting" `isInfixOf` bodyStr
        then return (Left "Invalid Hue key: got HTML response indicating no lighting")
        else return (Right body)

sendPostRequestNoAuth :: String -> Aeson.Value -> IO (Either String String)
sendPostRequestNoAuth url jsonPayload = do
  manager <- getInsecureManager
  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS $ Aeson.encode jsonPayload
        }
  response <- httpLbs request manager
  putStrLn "End of POST fn... POST Response:"
  L8.putStrLn $ responseBody response

  let responseBodyValue = Aeson.decode (responseBody response) :: Maybe [Aeson.Value]
  case responseBodyValue of
    Just [Aeson.Object obj] ->
      case (KM.lookup "success" obj, KM.lookup "error" obj) of
        (Just (Aeson.Object successObj), _) ->
          case KM.lookup "username" successObj of
            Just (Aeson.String username) -> return $ Right $ T.unpack username
            _ -> return $ Left "Success but no username found."
        (_, Just (Aeson.Object errorObj)) ->
          case KM.lookup "description" errorObj of
            Just (Aeson.String desc) -> return $ Left $ T.unpack desc
            _ -> return $ Left "Error occurred but no description provided."
        _ -> return $ Left "Unexpected response format."
    _ -> return $ Left "Failed to parse response as expected JSON."


-- Extract IDs from response body
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

-- Finds bridges
findBridgesOnNetwork :: IO (Either String [HueBridge])
findBridgesOnNetwork = do
  request <- parseRequest "https://discovery.meethue.com/"
  response <- httpLbs request =<< getInsecureManager
  let body = responseBody response
  return $ Aeson.eitherDecode body

-- Gets IP
getMainBridgeIp :: IO (Either String String)
getMainBridgeIp = do
  result <- findBridgesOnNetwork
  case result of
    Left err -> return $ Left $ "Failed to decode JSON: " ++ err
    Right [] -> return $ Left "No Hue Bridges found."
    Right (bridge:_) -> return $ Right (internalipaddress bridge)

apiHueUrl :: String -> String -> String
apiHueUrl ip param = "https://" ++ ip ++ "/clip/v2/resource" ++ param

apiLightUrl :: String -> String -> String
apiLightUrl hueUrl lightId = hueUrl ++ "/" ++ lightId

genKey :: String -> FilePath -> IO String
genKey ip keyFilePath = do
  let genKeyUrl = "https://" ++ ip ++ "/api"
  result <- sendPostRequestNoAuth genKeyUrl buildJsonPayloadGenKey
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
      putStrLn "Push the link button on the Hue Bridge and then press any key to try again..."
      _ <- getLine
      genKey ip keyFilePath
    Right key -> do
      writeFile keyFilePath key
      return key

data InputValue
  = S   String
  | I   Int
  | SI  String Int
  | SS  String String
  | III Int Int Int
  deriving (Show)

-- -- Extract String from S
-- extractS :: InputValue -> Maybe String
-- extractS (S s) = Just s
-- extractS _      = Nothing

-- -- Extract Int from I
-- extractI :: InputValue -> Maybe Int
-- extractI (I i) = Just i
-- extractI _     = Nothing

-- -- Extract (String, Int) from SI
-- extractSI :: InputValue -> Maybe (String, Int)
-- extractSI (SI s i) = Just (s, i)
-- extractSI _        = Nothing

-- -- Extract (String, String) from SS
-- extractSS :: InputValue -> Maybe (String, String)
-- extractSS (SS s1 s2) = Just (s1, s2)
-- extractSS _          = Nothing

handleInput :: String -> IO InputValue
handleInput input =
  case words input of
    [s]
      | all isAlpha s || (isAlpha (head s) && all isDigit (tail s)) -> return (S s)
      | Just i <- readMaybe s -> return (I i)

    [s, numStr]
      | Just i <- readMaybe numStr, i >= 0, i <= 100 -> return (SI s i)

    [s1, s2] -> return $ SS s1 s2

    [i1, i2, i3]
      | Just r <- readMaybe i1
      , Just g <- readMaybe i2
      , Just b <- readMaybe i3 -> return $ III r g b

    _ -> return (S "")  -- default case, returns an empty string in case of invalid input

getListIndex :: [a] -> [Int]
getListIndex xs = [i | (i, _) <- zip [0..] xs]

getTrueIndexes :: [Bool] -> [Int]
getTrueIndexes boolList = [i | (i, True) <- zip [0..] boolList]

convertRgbToXy :: (Int, Int, Int) -> (Double, Double) -- CIE 1931 light color supported by Hue Bridge API
convertRgbToXy (r, g, b) = (xHat, yHat)
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b
    x      = 0.4124 * r' + 0.3576 * g' + 0.1805 * b'
    y      = 0.2126 * r' + 0.7152 * g' + 0.0722 * b'
    z      = 0.0193 * r' + 0.1192 * g' + 0.9505 * b'
    total  = x + y + z
    (xHat, yHat)
      | total == 0 = (0,0)
      | otherwise  = (x / total, y / total)

main :: IO ()
main = do
  let ipFilePath = "ip.txt"
      keyFilePath = "key.txt"

  -- Read or create ip.txt file
  ipFileContent <- readOrCreateFile ipFilePath
  ip <- if ipFileContent == "" -- File does not exist or is empty
          then do
            bridge <- getMainBridgeIp -- Gets the ip from the API 
            case bridge of
              Left err  -> putStrLn err >> return "" -- Some error, return nothing
              Right ip' -> writeFile ipFilePath ip' >> return ip' -- IP was found, save it to the file
          else return ipFileContent
  putStrLn ip

  -- Read or create key.txt file
  keyFileContent <- readOrCreateFile keyFilePath
  key <- if keyFileContent == "" -- file does not exist or is empty
           then genKey ip keyFilePath -- user must press the Hue Bridge link button to generate a key and continue
           else return keyFileContent -- key was generated or already existed
  putStrLn key

  manager <- getInsecureManager -- HTTP request SSL disabled
  loop manager key ip -- print lights/states/bri and await for user input

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
              bri' = show (round (bri !! idx) :: Int)  -- rounding the brightness value to remove decimal
              -- Pad the name to the max length, left-align it
              paddedName = name ++ replicate (maxNameLength - length name) ' '
              -- Adjust state padding, ensuring fixed width for the state column
              paddedState = replicate (maxStateLength - length state) ' ' ++ state  -- Right-align 'state' in a 5-character wide column
              -- Adjust bri padding, ensuring fixed width for the bri column
              paddedBri = replicate (maxBriLength - length bri') ' ' ++ bri'  -- Right-align 'bri' in a 5-character wide column
          in putStrLn $ " | " ++ paddedName ++ "  |  " ++ paddedState ++ "  |  " ++ paddedBri ++ "  |  "
        ) customIdxs
      putStrLn " |            |         |       |"
      putStrLn " |---|λHue|---|--|0.1|--|--|?|--|"


-- |----- NAME -----|-- STATE --|-- BRI --|
-- |                |           |         |
-- |   Bed          |   false   |   100   |
-- |   Desk         |   false   |   100   |
-- |   Ceiling 1    |   false   |   100   |
-- |   Ceiling 2    |   false   |   100   |
-- |   Ceiling 3    |   false   |   100   |
-- |   Ceiling 4    |   false   |   100   |
-- |                |           |         |
-- |-----{pRue}-----|--{v1.0b}--|---{?}---|



      let lightsIdx     = getListIndex ids
          lightsOnList  = getTrueIndexes states
          lightsOffList = lightsIdx \\ lightsOnList -- difference of Sets operator (\\) e.g. l = [1,2,3] and l' = [2] .:. l \\ l' = [1,3]

      putStrLn $ "\n" ++ "light id, idx"
      print $ zip ids lightsIdx
      -- print lightsIdxs
      putStrLn $ "\n" ++ "idx on list"
      print lightsOnList
      putStrLn $ "\n" ++ "idx off list"
      print lightsOffList

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
      --   Hue Light Order   --
      -------------------------
      --  [  CEILING 2  ]     0
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
