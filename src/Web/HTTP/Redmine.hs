{-# LANGUAGE OverloadedStrings #-}
{-|
Module          : Web.HTTP.Redmine
Description     : An API Library for the Redmine Bug Tracker
Copyright       : (c) Pavan Rikhi, 2014
License         : GPL-3
Maintainer      : pavan@sleepanarchy.com
Stability       : experimental
Portability     : POSIX

Redmine is a library for interacting with the API for Redmine, a bug
tracker written in Ruby.

Actions are run in the 'Redmine' Monad, which should be supplied with
a 'RedmineConfig'. You will need to override the default 'redAPI' and
'redURL' Config fields.

The following actions are currently supported:

* Fetching All Projects
* Fetching All/Personal Issues
* Fetching All Issue Statuses
* Fetching a Specific Issue Status by Name or Id
* Updating an Issue [TODO: more heavylifting in the library(vs user code)]

This is not intended to be a complete API library, just what is required
for `hkredmine`. However, it _could_ be a complete API library,
contributions are welcome...

-}
module Web.HTTP.Redmine
        (
        -- * Redmine Monad
          Redmine
        , runRedmine
        , defaultRedmineConfig
        , redmineLeft
        -- * Redmine Types
        , ProjectId
        , Project(..)
        , Projects(..)
        , IssueId
        , Issue(..)
        , Issues(..)
        , Status(..)
        , User(..)
        -- ** API-related Types
        , RedmineConfig(..)
        -- * Redmine API Functions
        -- ** Projects
        , getProjects
        -- ** Issues
        , getAllIssues
        , getMyIssues
        , getIssue
        , updateIssue
        -- ** Issue Statuses
        , getStatuses
        , getStatusFromName
        , getStatusFromId
        -- ** Misc
        , getCurrentUser
        , addWatcher
        , removeWatcher
        -- * Formatting
        , projectsTable
        , projectDetail
        , issuesTable
        ) where

import Data.Aeson                               (FromJSON, object, (.=), encode)
import qualified Data.ByteString.Char8 as BC    (pack)
import qualified Data.ByteString.Lazy as LB     (ByteString)
import qualified Data.List as L                 (find)

import Web.HTTP.Redmine.Client
import Web.HTTP.Redmine.Format
import Web.HTTP.Redmine.Types


-- Projects
-- | Retrieve All the 'Projects'
getProjects :: Redmine Projects
getProjects                 = getEndPoint GetProjects []


-- Issues
-- | Retrieve All 'Issues' of a 'Project'
getAllIssues :: ProjectId -> Redmine Issues
getAllIssues projectID      = getEndPoint GetIssues
        [ ("project_id", BC.pack $ show projectID)
        ]

-- | Retrieve 'Issues' of a 'Project' assigned to the user
getMyIssues :: ProjectId -> Redmine Issues
getMyIssues projectID       = getEndPoint GetIssues
        [ ("project_id", BC.pack $ show projectID)
        , ("assigned_to_id", "me")
        , ("offset", "0")
        , ("limit", "100")
        ]

-- | Retrieve an 'Issue'.
getIssue :: IssueId -> Redmine Issue
getIssue issueID            = getEndPoint (GetIssue issueID) []

-- | Update an 'Issue'.
updateIssue ::  IssueId -> LB.ByteString -> Redmine ()
updateIssue issueID         = putEndPoint $ UpdateIssue issueID

-- Statuses
-- | Retrieve All Available Statuses
getStatuses :: Redmine [Status]
getStatuses                 = do (Statuses ss) <- getEndPoint GetStatuses []
                                 return ss

-- | Retrieve the 'Status' with the given name.
getStatusFromName :: String -> Redmine (Maybe Status)
getStatusFromName name      = getStatusFromField ((== name) . statusName)

-- | Retrieve the 'Status' with the given id.
getStatusFromId :: Integer -> Redmine (Maybe Status)
getStatusFromId i           = getStatusFromField ((== i) . statusId)

-- | Retrieve the 'Status' using the given predicate.
getStatusFromField :: (Status -> Bool) -> Redmine (Maybe Status)
getStatusFromField          = getItemFromField getStatuses

-- | Search a 'Redmine' type using the given predicate.
getItemFromField :: FromJSON a => Redmine [a] -> (a -> Bool) -> Redmine (Maybe a)
getItemFromField items p    = fmap (L.find p) items

-- Users
-- | Retrieve the current 'User'.
getCurrentUser :: Redmine User
getCurrentUser = getEndPoint GetCurrentUser []

-- Watching
-- | Add a watcher to an 'Issue'.
addWatcher :: IssueId -> User -> Redmine ()
addWatcher i user           = postEndPoint (AddWatcher i) postData
        where postData      = encode $ object [ "user_id" .= userId user ]

-- | Remove a watcher from an 'Issue'.
removeWatcher :: IssueId -> User -> Redmine ()
removeWatcher i user        = deleteEndPoint $ RemoveWatcher i $ userId user
