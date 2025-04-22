{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import System.Directory (doesFileExist)
import Network.HTTP.Client
  ( Request(..), RequestBody(..), method, requestBody
  , parseRequest, Manager, httpLbs, responseBody
  )
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS ( ClientParams(..), defaultParamsClient, Shared(..), ValidationCache(..)  )
import Data.X509.Validation (ValidationCacheResult(..)) -- <- ValidationCachePass lives here
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS (newTlsManagerWith)
import Data.Default.Class (def)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as BS
-- import Data.ByteString
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object)
import Network.HTTP.Types.Method (methodPut)

-- import Data.List (isInfixOf)
-- import Control.Monad (unless)
-- import Data.Maybe (fromMaybe)
-- import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
-- import Network.HTTP.Client.Conduit (bodyReaderSource)
-- import Network.HTTP.Types (Status(statusMessage))
-- import Control.Monad (forM)
import Control.Concurrent.Async (mapConcurrently_)


-- Represents the HueBridge
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
  , "dynamics" .= object [ "duration" .= (0 :: Int) ] -- Transition time hard coded to 0 for the fastest speed possible
  ]

-- JSON payload creator
buildJsonPayloadLightsOff :: Aeson.Value
buildJsonPayloadLightsOff = object
  [ "on"       .= object [ "on" .= False ]
  , "dynamics" .= object [ "duration" .= (0 :: Int) ]
  ]

buildJsonPayloadLightsOn :: Aeson.Value
buildJsonPayloadLightsOn = object
  [ "on"       .= object [ "on" .= True ]
  , "dynamics" .= object [ "duration" .= (0 :: Int) ]
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

-- Sends the PUT request
sendPutRequest :: String -> String -> Aeson.Value -> IO ()
sendPutRequest key url jsonPayload = do
  manager <- getInsecureManager
  initialRequest <- parseRequest url
  let request = initialRequest
        { method         = methodPut
        , requestBody    = RequestBodyLBS $ Aeson.encode jsonPayload
        , requestHeaders = [("hue-application-key", BS.pack key)] 
        }
  response <- httpLbs request manager
  putStrLn "End of PUT fn... PUT Response:"
  L8.putStrLn $ responseBody response

-- Sends the GET request
-- sendGetRequest :: String -> String -> IO ()
-- sendGetRequest key url = do
--   manager <- getInsecureManager
--   initialRequest <- parseRequest url
--   let request = initialRequest
--         { method         = "GET"
--         , requestHeaders = [("hue-application-key", BS.pack key)]
--         }
--   response <- httpLbs request manager
--   putStrLn "GET Response:"
--   L8.putStrLn $ responseBody response
-- Modified GET request that returns body
sendGetRequest :: String -> String -> IO L8.ByteString
sendGetRequest key url = do
  manager <- getInsecureManager
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



-- Sends a POST request without the Hue API header
-- used to gen a key
-- sendPostRequestNoAuth :: String -> Aeson.Value -> IO (Either String String)
-- sendPostRequestNoAuth url jsonPayload = do
--   manager <- getInsecureManager
--   initialRequest <- parseRequest url
--   let request = initialRequest
--         { method         = "POST"
--         , requestBody    = RequestBodyLBS $ Aeson.encode jsonPayload
--         -- , requestHeaders = [("Content-Type", "application/json")]
--         }
--   response <- httpLbs request manager
--   putStrLn "POST Response:"
--   L8.putStrLn $ responseBody response
--   return $ Right $ L8.unpack $ responseBody response

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

mkHueApiUrl :: String -> String -> String
mkHueApiUrl ip param = "https://" ++ ip ++ "/clip/v2/resource" ++ param

mkHueLightsUrl :: String -> String -> String
mkHueLightsUrl baseUrl lightId = baseUrl ++ "/" ++ lightId

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
  -- keyFileContent <- readOrCreateFile keyFilePath
  -- key <- if keyFileContent == "" -- File does not exist or is empty
  --         then do
  --          let genKeyUrl = "https://" ++ ip ++ "/api"
  --          result <- sendPostRequestNoAuth genKeyUrl buildJsonPayloadGenKey -- Generate a new key with a POST request, user must press the button on Bridge to continue
  --          case result of
  --            Left err   -> putStrLn err >> return "" -- Some error, return nothing
  --            Right key' -> writeFile keyFilePath key' >> return key' -- Key was successfully generated, save it to the file
  --        else return keyFileContent
  
  -- Read or create key.txt file
  keyFileContent <- readOrCreateFile keyFilePath
  key <- if keyFileContent == ""
           then genKey ip keyFilePath
           else return keyFileContent
  putStrLn key

  let url = mkHueApiUrl ip "/light"
  body <- sendGetRequest key url
  let ids = extractIdsFromBody body
  let names = extractNamesFromBody body
  let states = extractStatesFromBody body
  let bri = extractBriFromBody body
  print ids
  print names
  print states
  print bri
  -- let url' = url ++ "/" ++ ids !! 2
  -- putStrLn url'
  -- print $ ids !! 2
  -- sendPutRequest key url' $ buildJsonPayloadLightsToggle True
  let toggleLight idx = sendPutRequest key (mkHueLightsUrl url (ids !! idx)) $ buildJsonPayloadLightsToggle $ not $ states !! idx
  userInput <- getLine
  case userInput of
    -- let deskUrl = mkHueLightsUrl url $ ids !! 4
    "d"  -> sendPutRequest key (mkHueLightsUrl url (ids !! 2)) $ buildJsonPayloadLightsToggle $ not $ states !! 2 -- Toggle DESK light
    "b"  -> sendPutRequest key (mkHueLightsUrl url (ids !! 4)) $ buildJsonPayloadLightsToggle $ not $ states !! 4 -- Toggle BED  light
    "c1" -> sendPutRequest key
                           (mkHueLightsUrl url (ids !! 5)) $
                           buildJsonPayloadLightsToggle $ not $ states !! 5
    "c2" -> sendPutRequest key
                           (mkHueLightsUrl url (ids !! 0)) $
                           buildJsonPayloadLightsToggle $ not $ states !! 0
    "c3" -> sendPutRequest key
                           (mkHueLightsUrl url (ids !! 3)) $
                           buildJsonPayloadLightsToggle $ not $ states !! 3
    "c4" -> sendPutRequest key
                           (mkHueLightsUrl url (ids !! 1)) $
                           buildJsonPayloadLightsToggle $ not $ states !! 1
    "c"  -> mapConcurrently_ toggleLight [5,0,3,1] -- Send the request to each CEILING lights in parallel to speed the toggle
    "db" -> mapConcurrently_ toggleLight [2,4] -- Send the request to DESK and BED lights in parallel to speed the toggle
    _   -> return ()
  main

-- ["Ceiling 2","Ceiling 4","Desk","Ceiling 3","Bed","Ceiling 1"]

  -- results <- forM ids $ \rid -> d
  --   let url' = url ++ "/" ++ rid
  --   putStrLn url'
  --   body <- sendGetRequest key url'
  --   let states = extractStatesFromBody body
  --   return states
  
  -- let allStates = concat results
  -- print allStates 

  -- let url' = url ++ "/" ++ "5a3c6903-f1e1-44b9-ae33-e89ed65651f8"
  -- sendGetRequest key url' >>= print




  -- keyFileContent <- readOrCreateFile keyFilePath
  -- if keyFileContent == "" then
  --   let genKeyUrl = "https://" ++ ip ++ "/api"
  --       genKey    = sendPostRequestNoAuth genKeyUrl buildJsonPayloadGenKey
  --   in genKey
  --   else return ()

  -- keyFileContent <- readOrCreateFile keyFilePath
  -- key <- if keyFileContent == "" then do
  --          let genKeyUrl = "https://" ++ ip ++ "/api"
  --          result <- sendPostRequestNoAuth genKeyUrl buildJsonPayloadGenKey
  --          case result of
  --            Left err -> putStrLn err >> return ""
  --            Right key' -> writeFile keyFilePath key' >> return key'
  --        else return keyFileContent

  -- putStrLn key

  -- POST Response:
  -- [{"error":{"type":101,"address":"","description":"link button not pressed"}}]
  
  -- POST Response:
  -- [{"success":{"username":"QYB7U17TDDHb9dQ0AUuHqUJBXU0xSPB-e67kZznm","clientkey":"40202CE3F4FCCD491F37872A52DA991B"}}]
