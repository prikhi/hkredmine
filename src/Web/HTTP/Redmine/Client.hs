{-# LANGUAGE OverloadedStrings #-}
{-|
-
- This Module contains functions related to interacting with the a Redmine
- server, such as creating a default connection configuration, querying
- urls and POSTing data.
-
-}
module Web.HTTP.Redmine.Client
        ( Redmine
        , runRedmine
        , RedmineConfig(..)
        , defaultRedmineConfig
        , getEndPoint
        , postEndPoint
        ) where

import Control.Exception.Lifted         (catch, throwIO)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Logger             (runStderrLoggingT, LoggingT)
import Control.Monad.State              (evalStateT, StateT, get)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Either       (runEitherT, EitherT, hoistEither, left)
import Control.Monad.Trans.Resource     (runResourceT, ResourceT)
import Data.Aeson                       (eitherDecode, FromJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Conduit
import Network.HTTP.Types               (statusCode, statusMessage)

import Web.HTTP.Redmine.Types


-- | The Redmine Monad Holds the Configuration State and Allows Network and
-- Logging Actions
type Redmine a           = LoggingT (StateT RedmineConfig (ResourceT (EitherT String IO))) a

-- | Run a 'Redmine' Action
runRedmine :: MonadIO m => RedmineConfig -> Redmine a -> m (Either String a)
runRedmine config action = liftIO . runEitherT . runResourceT .
                          flip evalStateT config .  runStderrLoggingT $ action

-- | Represents the Current Redmine Configuration State
data RedmineConfig       = RedmineConfig
                         { -- | The User's API Key
                           redAPI    :: B.ByteString
                           -- | The Desired API URL
                         , redURL     :: String
                           -- | The Connection Manager for All Requests
                         , redManager :: Manager
                         }

-- | Creates a 'RedmineConfig' with a new Manager
defaultRedmineConfig :: MonadIO m => m RedmineConfig
defaultRedmineConfig     = do
        man <- liftIO $ newManager conduitManagerSettings
        return RedmineConfig { redAPI     = ""
                             , redURL     = ""
                             , redManager = man }

getEndPoint :: FromJSON a =>
                EndPoint -> [(B.ByteString, B.ByteString)] -> Redmine a
getEndPoint ep getData = do
        config   <- get
        initReq  <- liftIO $ parseUrl $ makeURL (redURL config) ep ++ getParams
        let redReq  = initReq { requestHeaders =
                                            [ ("Content-Type", "application/json")
                                            , ("X-Redmine-API-Key", redAPI config)]
                                      , method         = "GET" }
        makeRequest config redReq
        where getParams = "?" ++ concatMap (\(a, v) -> "&" ++ BC.unpack a ++ "=" ++ BC.unpack v) getData

-- | Queries an API 'EndPoint' with a POST Request
postEndPoint :: FromJSON a =>
                EndPoint -> [(B.ByteString, B.ByteString)] -> Redmine a
postEndPoint ep postData = do
        config   <- get
        initReq  <- liftIO $ parseUrl $ makeURL (redURL config) ep
        let postReq = urlEncodedBody postData initReq
            redReq  = postReq { requestHeaders =
                                    [ ("Content-Type", "application/json")
                                    , ("X-Redmine-API-Key", redAPI config)]
                              , method         = "GET" }
        makeRequest config redReq
makeRequest :: FromJSON a => RedmineConfig -> Request -> Redmine a
makeRequest config request = do
        response <- catch (httpLbs request $ redManager config)
            (\e -> case e :: HttpException of
                StatusCodeException status _ _ -> lift . lift . lift . left $
                        "Status Code: " ++ show (statusCode status) ++ "\n" ++
                        "Status Message: " ++ show (statusMessage status)
                _                              -> throwIO e)
        lift . lift . lift . hoistEither . eitherDecode . responseBody $ response


-- | Builds the URL for the 'EndPoint'
makeURL :: String -> EndPoint -> String
makeURL url e         = url ++ endpoint e ++ ".json"
        where endpoint GetProjects = "projects"
              endpoint GetIssues   = "issues"
