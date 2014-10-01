{-# LANGUAGE OverloadedStrings #-}
{-|
-
- This Module contains functions related to interacting with the a Redmine
- server, such as creating a default connection configuration, querying
- urls and POSTing data.
-
-}
module Web.HTTP.Redmine.Client
        ( getEndPoint
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
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.State              (get)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Either       (hoistEither)
import Data.Aeson                       (eitherDecode, FromJSON)
import Network.HTTP.Conduit
import Network.HTTP.Types               (statusCode, statusMessage)

import Web.HTTP.Redmine.Types
import Web.HTTP.Redmine.Monad


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

-- | Build the full URL from a base redURL' and an 'EndPoint'.
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
              endpoint (GetCategories p)        =  "projects/" ++ show p ++ "/issue_categories"
