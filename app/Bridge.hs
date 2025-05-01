module Bridge where

import Types    (HueBridge, internalipaddress)
import Network  (getInsecureManager, sendPostRequestNoAuth)
import Payloads (buildJsonPayloadGenKey)
import Network.HTTP.Client ( parseRequest, httpLbs, responseBody, Response (responseBody) )
import qualified Data.Aeson as Aeson

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
    -- TODO if the there is more than one Bridge connected, either make the user choose OR send a request to all Bridges and return only the one that responds

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


apiHueUrl :: String -> String -> String
apiHueUrl ip param = "https://" ++ ip ++ "/clip/v2/resource" ++ param

apiLightUrl :: String -> String -> String
apiLightUrl hueUrl lightId = hueUrl ++ "/" ++ lightId

