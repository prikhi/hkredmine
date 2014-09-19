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
        , redmineLeft
        , getEndPoint
        , postEndPoint
        , putEndPoint
        ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC

import Control.Exception.Lifted         (catch, throwIO)
import Control.Monad                    (void)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Control.Monad.Logger             (runStderrLoggingT, LoggingT)
import Control.Monad.State              (evalStateT, StateT, get)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Either       (runEitherT, EitherT, hoistEither, left)
import Control.Monad.Trans.Resource     (runResourceT, ResourceT)
import Data.Aeson                       (eitherDecode, FromJSON)
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

-- | Return an irrecoverable error in the 'Redmine' Monad.
redmineLeft :: String -> Redmine a
redmineLeft = lift . lift . lift . left

-- | Send a GET request to the given 'EndPoint' along with any passed
-- parameters
getEndPoint :: FromJSON a =>
                EndPoint -> [(B.ByteString, B.ByteString)] -> Redmine a
getEndPoint ep getData = do
        config      <- get
        initReq     <- liftIO $ parseUrl $ makeURL (redURL config) ep ++ getParams
        let redReq  = initReq { requestHeaders =
                                    [ ("Content-Type", "application/json")
                                    , ("X-Redmine-API-Key", redAPI config)]
                              , method         = "GET" }
        response    <- makeRequest redReq
        lift . lift . lift . hoistEither . eitherDecode . responseBody $ response
        where getParams = "?" ++ concatMap (\(a, v) -> "&" ++ BC.unpack a ++
                                                       "=" ++ BC.unpack v) getData

-- | Send a POST request to the given 'EndPoint' along with any passed
-- parameters
postEndPoint :: EndPoint -> [(B.ByteString, B.ByteString)] -> Redmine ()
postEndPoint ep postData = do
        config   <- get
        initReq  <- liftIO $ parseUrl $ makeURL (redURL config) ep
        let postReq = urlEncodedBody postData initReq
            redReq  = postReq { requestHeaders =
                                    [ ("Content-Type", "application/json")
                                    , ("X-Redmine-API-Key", redAPI config)]
                              , method         = "POST" }
        void $ makeRequest redReq


-- | Send a PUT request to the given 'EndPoint' along with any passed
-- parameters
putEndPoint :: EndPoint -> String -> Redmine ()
putEndPoint ep putData  = do
        config   <- get
        initReq  <- liftIO $ parseUrl $ makeURL (redURL config) ep
        let redReq  = initReq { requestHeaders  =
                                    [ ("Content-Type", "application/json")
                                    , ("X-Redmine-API-Key", redAPI config)]
                              , requestBody     = RequestBodyBS . BC.pack $ putData
                              , method          = "PUT"
                              }
        void $ makeRequest redReq


-- | Send a Request to a Redmine Instance
makeRequest :: Request -> Redmine (Response LBC.ByteString)
makeRequest request = do
        config   <- get
        catch (httpLbs request $ redManager config)
            (\e -> case e :: HttpException of
                StatusCodeException status _ _ -> redmineLeft $
                        "Status Code: " ++ show (statusCode status) ++ "\n" ++
                        "Status Message: " ++ show (statusMessage status)
                _                              -> throwIO e)


-- | Builds the URL for the 'EndPoint'
makeURL :: String -> EndPoint -> String
makeURL url e         = url ++ endpoint e ++ ".json"
        where endpoint GetProjects      = "projects"
              endpoint GetIssues        = "issues"
              endpoint GetStatuses      = "issue_statuses"
              endpoint (GetIssue i)     = "issues/" ++ show i
              endpoint (UpdateIssue i)  = "issues/" ++ show i
