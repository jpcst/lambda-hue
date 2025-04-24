{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import LambdaHue.Utils

import System.Directory (doesFileExist)
import Network.HTTP.Client
  ( Request(..), RequestBody(..), method, requestBody
  , parseRequest, Manager, httpLbs, responseBody
  )
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.TLS ( ClientParams(..), defaultParamsClient, Shared(..), ValidationCache(..)  )
import Data.X509.Validation (ValidationCacheResult(..)) -- <- ValidationCachePass lives here
import Network.Connection (TLSSettings(..))
import Data.Default.Class (def)
import qualified Data.ByteString.Lazy.Char8 as L8
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

buildJsonPayloadSetBri :: Int -> Int -> Aeson.Value
buildJsonPayloadSetBri transitionTime bri = object
  [ "dimming"       .= object [ "brightness" .= bri ]
  , "dynamics" .= object [ "duration" .= transitionTime ] -- Default transition time is 400 (ms). Set to 0 for max speed
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

sendPutRequest :: Manager -> String -> String -> Aeson.Value -> IO ()
sendPutRequest manager key url jsonPayload = do
  initialRequest <- parseRequest url
  let request = initialRequest
        { method         = methodPut
        , requestBody    = RequestBodyLBS $ Aeson.encode jsonPayload
        , requestHeaders = [("hue-application-key", BS.pack key)] 
        }
  response <- httpLbs request manager
  putStrLn "End of PUT fn... PUT Response:"
  L8.putStrLn $ responseBody response

sendGetRequest :: Manager -> String -> String -> IO L8.ByteString
sendGetRequest manager key url = do
  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "GET"
        , requestHeaders = [("hue-application-key", BS.pack key)]
        }
  response <- httpLbs request manager
  putStrLn "End of GET fn... GET Response:"
  let body = responseBody response
  -- L8.putStrLn body
  return body

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
  = S String
  | I Int
  | SI String Int
  | SS String String
  deriving (Show)

-- Extract String from S
extractS :: InputValue -> String
extractS (S s) = s

-- Extract Int from I
extractI :: InputValue -> Int
extractI (I i) = i

-- Extract (String, Int) from SI
extractSI :: InputValue -> (String, Int)
extractSI (SI s i) = (s, i)

-- Extract (String, String) from SS
extractSS :: InputValue -> (String, String)
extractSS (SS s1 s2) = (s1, s2)

handleInput :: String -> IO InputValue
handleInput input =
  case words input of
    [s]
      | all isAlpha s || (isAlpha (head s) && all isDigit (tail s)) -> return (S s)
      | Just i <- readMaybe s -> return (I i)

    [s, numStr]
      | Just i <- readMaybe numStr, i >= 0, i <= 100 -> return (SI s i)

    [s1, s2] -> return (SS s1 s2)

    _ -> return (S "")  -- default case, returns an empty string in case of invalid input

getListIndex :: [a] -> [Int]
getListIndex xs = [i | (i, _) <- zip [0..] xs]

getTrueIndexes :: [Bool] -> [Int]
getTrueIndexes boolList = [i | (i, True) <- zip [0..] boolList]


main :: IO ()
main = do
  -- test <- getLine
  -- ipt <- handleInput test
  -- case ipt of
  --   SS s1 s2 -> putStrLn $ s1 ++ s2

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
  loop manager key ip -- print lights/states/bri and await for user inputs

loop :: Manager -> String -> String -> IO ()
loop manager key ip = do
  let hueUrl = apiHueUrl ip "/light" -- https://<ip>/clip/v2/resource/ ++ light
  body <- sendGetRequest manager key hueUrl
  let ids    = extractIdsFromBody body
      names  = extractNamesFromBody body
      states = extractStatesFromBody body
      bri    = extractBriFromBody body
  print ids
  print names
  print states
  print bri

  let lightsIdx = getListIndex ids
      lightsOnList = getTrueIndexes states

  -- change this todo
  let toggleLight position = sendPutRequest manager
                                            key
                                            (apiLightUrl hueUrl (ids !! position)) $ -- https://<ip>/clip/v2/resource/light ++ /<lightid>
                                            buildJsonPayloadLightsToggle $ not $ states !! position
  let setLightBri tt bri position = sendPutRequest manager
                                                   key
                                                   (apiLightUrl hueUrl (ids !! position)) $
                                                   buildJsonPayloadSetBri tt bri
  let setLightXy tt (x,y) position = sendPutRequest manager
                                                    key
                                                    (apiLightUrl hueUrl (ids !! position)) $
                                                    buildJsonPayloadSetXy tt (x,y)

  userInput <- getLine
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
    S "c"  -> mapConcurrently_ toggleLight [5,0,3,1] -- Send the request to each CEILING light in parallel
    S "db" -> mapConcurrently_ toggleLight [2,4]     -- Send the request to DESK and BED lights in parallel

    S "br" -> mapConcurrently_(setLightXy 0 (0.3,0.3)) lightsOnList
    S "am" -> mapConcurrently_(setLightXy 0 (0.5019, 0.4152)) lightsOnList

    I bri  -> mapConcurrently_ (setLightBri 0 bri) lightsOnList

    SI "d"  bri -> setLightBri 0 bri 2
    SI "b"  bri -> setLightBri 0 bri 4
    SI "c1" bri -> setLightBri 0 bri 5
    SI "c2" bri -> setLightBri 0 bri 0
    SI "c3" bri -> setLightBri 0 bri 3
    SI "c4" bri -> setLightBri 0 bri 1
    SI "c"  bri -> mapConcurrently_ (setLightBri 0 bri) [5,0,3,1]
    SI "db" bri -> mapConcurrently_ (setLightBri 0 bri) [2,4]

    -- SS light color -> putStrLn $ light ++ " " ++ color
    SS "d" "br" -> setLightXy 0 (0.3,0.3) 2

    _ -> return ()

  loop manager key ip
