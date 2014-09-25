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
        , deleteEndPoint
        ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
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


-- | A basic Redmine API Request. The API key is sent through both HTTP
-- authorization and the request's headers to make sure we support as many
-- servers as possible.
redmineRequest :: EndPoint -> Redmine Request
redmineRequest ep       = do
        config      <- get
        initReq     <- liftIO . parseUrl $ makeURL (redURL config) ep
        return . applyBasicAuth (redAPI config) "" $
               initReq { requestHeaders = [ ("Content-Type", "application/json")
                                          , ("X-Redmine-API-Key", redAPI config)
                                          ] }

-- | Send a GET request to the given 'EndPoint' along with any passed
-- parameters
getEndPoint :: FromJSON a =>
                EndPoint -> [(B.ByteString, B.ByteString)] -> Redmine a
getEndPoint ep getData  = do
        initReq         <- redmineRequest ep
        let getReq      = initReq { method = "GET"
                                  , queryString = getParams }
        response        <- makeRequest getReq
        lift . lift . lift . hoistEither . eitherDecode . responseBody $ response
        where getParams = BC.concat $ map (\(a, v) -> BC.concat ["&", a, "=", v])
                                          getData

-- | Send a POST request to the given 'EndPoint' along with any passed
-- parameters
postEndPoint :: EndPoint -> LB.ByteString -> Redmine (Response LBC.ByteString)
postEndPoint ep postData = do
        initReq         <- redmineRequest ep
        makeRequest initReq
                { method      = "POST"
                , requestBody = RequestBodyLBS postData
                }

-- | Send a PUT request to the given 'EndPoint' along with any passed
-- parameters
putEndPoint :: EndPoint -> LB.ByteString -> Redmine ()
putEndPoint ep putData          = do
        initReq                 <- redmineRequest ep
        void $ makeRequest initReq
                { method        = "PUT"
                , requestBody   = RequestBodyLBS putData
                }

-- | Send a DELETE request to the given 'EndPoint'.
deleteEndPoint :: EndPoint -> Redmine ()
deleteEndPoint ep               = do
        initReq                 <- redmineRequest ep
        void $ makeRequest initReq { method   = "DELETE" }


-- | Send a Request to a Redmine Instance
makeRequest :: Request -> Redmine (Response LBC.ByteString)
makeRequest request = do
        config      <- get
        catch (httpLbs request $ redManager config)
            (\e -> case e :: HttpException of
                StatusCodeException status _ _ -> redmineLeft $
                        "Status Code: " ++ show (statusCode status) ++ "\n" ++
                        "Status Message: " ++ show (statusMessage status)
                _                              -> throwIO e)


-- | Builds the URL for the 'EndPoint'
makeURL :: String -> EndPoint -> String
makeURL url e       = url ++ endpoint e ++ ".json"
        where endpoint GetProjects              = "projects"
              endpoint GetIssues                = "issues"
              endpoint (GetProjectsIssues n)    = "projects/" ++ show n ++ "/issues"
              endpoint GetStatuses              = "issue_statuses"
              endpoint (GetIssue i)             = "issues/" ++ show i
              endpoint (UpdateIssue i)          = "issues/" ++ show i
              endpoint GetCurrentUser           = "users/current"
              endpoint (AddWatcher i)           = "issues/" ++ show i ++ "/watchers"
              endpoint (RemoveWatcher i u)      = "issues/" ++ show i ++ "/watchers/"
                                                ++ show u
              endpoint (GetVersion v)           = "versions/" ++ show v
              endpoint (GetVersions n)          = "projects/" ++ show n ++ "/versions"
              endpoint GetActivites             = "enumerations/time_entry_activities"
              endpoint GetTimeEntries           = "time_entries"
              endpoint GetTrackers              = "trackers"
              endpoint GetPriorities            = "enumerations/issue_priorities"
