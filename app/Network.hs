module Network where

import Network.HTTP.Client       ( Request(..), RequestBody(..), method
                                 , requestBody, parseRequest, Manager
                                 , httpLbs, responseBody, Response(responseBody) )
import Network.HTTP.Client.TLS   (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Types.Method (methodPut, methodGet, methodPost)
import Network.TLS               (ClientParams(..), defaultParamsClient, Shared(..), ValidationCache(..))
import Network.Connection        (TLSSettings(..))
import qualified Data.Aeson        as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key    as AK
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8      as BS
import qualified Data.Text as T
import Data.List (isInfixOf)
import Control.Exception (try, SomeException)
import qualified Data.CaseInsensitive as CI
import Data.Default.Class (def)
import Data.X509.Validation (ValidationCacheResult(..))

sendPutRequest :: Manager -> String -> String -> Aeson.Value -> IO L8.ByteString
sendPutRequest manager key url jsonPayload = do
  initialRequest <- parseRequest url
  let request = initialRequest
        { method         = methodPut
        , requestBody    = RequestBodyLBS $ Aeson.encode jsonPayload
        , requestHeaders = [(CI.mk $ BS.pack "hue-application-key", BS.pack key)]
        }
  response <- httpLbs request manager
  -- putStrLn "End of PUT fn... PUT Response:"
  -- L8.putStrLn $ responseBody response
  return $ responseBody response

sendGetRequest :: Manager -> String -> String -> IO (Either String L8.ByteString)
sendGetRequest manager key url = do
  result <- try $ do
    initialRequest <- parseRequest url
    let request = initialRequest
          { method = methodGet
          , requestHeaders = [(CI.mk $ BS.pack "hue-application-key", BS.pack key)]
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

getInsecureManager :: IO Manager
getInsecureManager = do
  let clientParams = (defaultParamsClient "" $ BS.pack "")
        { clientSupported = def
        , clientShared = def
            { sharedValidationCache = ValidationCache
                (\_ _ _ -> return ValidationCachePass)
                (\_ _ _ -> return ())
            }
        }
  newTlsManagerWith $ mkManagerSettings (TLSSettings clientParams) Nothing

sendPostRequestNoAuth :: String -> Aeson.Value -> IO (Either String String)
sendPostRequestNoAuth url jsonPayload = do
  manager <- getInsecureManager
  initialRequest <- parseRequest url
  let request = initialRequest
        { method = methodPost
        , requestBody = RequestBodyLBS $ Aeson.encode jsonPayload
        }
  response <- httpLbs request manager
  putStrLn "End of POST fn... POST Response:"
  L8.putStrLn $ responseBody response

  let responseBodyValue = Aeson.decode (responseBody response) :: Maybe [Aeson.Value]
  case responseBodyValue of
    Just [Aeson.Object obj] ->
      case (KM.lookup (AK.fromText (T.pack "success")) obj, KM.lookup (AK.fromText (T.pack "error")) obj) of
        (Just (Aeson.Object successObj), _) ->
          case KM.lookup (AK.fromText (T.pack "username")) successObj of
            Just (Aeson.String username) -> return $ Right $ T.unpack username
            _ -> return $ Left "Success but no username found."
        (_, Just (Aeson.Object errorObj)) ->
          case KM.lookup (AK.fromText (T.pack "description")) errorObj of
            Just (Aeson.String desc) -> return $ Left $ T.unpack desc
            _ -> return $ Left "Error occurred but no description provided."
        _ -> return $ Left "Unexpected response format."
    _ -> return $ Left "Failed to parse response as expected JSON."
