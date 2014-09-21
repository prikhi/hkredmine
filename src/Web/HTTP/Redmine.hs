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
* Updating an Issue [TODO: more heavylifting in the library(less user code)]
* Add/Remove Watchers from Images

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
        -- ** ID Types
        , ProjectId
        , ProjectIdent
        , IssueId
        , VersionId
        , Project(..)
        , Projects(..)
        , Issue(..)
        , Issues(..)
        , Status(..)
        , User(..)
        , Version(..)
        -- ** API-related Types
        , RedmineConfig(..)
        -- * Redmine API Functions
        -- ** Projects
        , getProjects
        , getProjectFromIdent
        -- ** Issues
        , getAllIssues
        , getMyIssues
        , getProjectsIssues
        , getVersionsIssues
        , getIssue
        , updateIssue
        -- ** Issue Statuses
        , getStatuses
        , getStatusFromName
        , getStatusFromId
        -- ** Versions
        , getVersions
        , getVersion
        , getNextVersionDue
        -- ** Misc
        , getCurrentUser
        , addWatcher
        , removeWatcher
        -- * Formatting
        , projectDetail
        , versionDetail
        , projectsTable
        , issuesTable
        , versionTable
        ) where

import qualified Data.ByteString.Char8 as BC    (pack)
import qualified Data.ByteString.Lazy as LB     (ByteString)
import qualified Data.List as L                 (find, sortBy)

import Control.Monad                (when)
import Data.Aeson                   (FromJSON, object, (.=), encode)
import Data.Function                (on)
import Data.Maybe                   (isJust, isNothing, fromJust)
import Safe                         (headMay)

import Web.HTTP.Redmine.Client
import Web.HTTP.Redmine.Format
import Web.HTTP.Redmine.Types


-- Projects
-- | Retrieve all 'Projects'.
getProjects :: Redmine [Project]
getProjects                 = do Projects ps <- getEndPoint GetProjects []
                                 return ps

-- | Attempt to retrieve a 'Project'  from a given 'ProjedtIdent'. String
-- versions of a 'ProjectId' are also accepted.
getProjectFromIdent :: ProjectIdent -> Redmine Project
getProjectFromIdent pIdent = do
        maybeProject <- getItemFromField getProjects
                                         (\p -> pIdent `elem` [ projectIdentifier p
                                                              , show $ projectId p])
        when (isNothing maybeProject) $ redmineLeft "Not a valid Project Identifier."
        return $ fromJust maybeProject


-- Issues
-- | Retrieve all 'Issues'.
getAllIssues :: IssueFilter -> Redmine [Issue]
getAllIssues f              = do
        Issues is <- getEndPoint GetIssues $ [ ("offset", "0")
                                            , ("limit", "100")
                                            ] ++ f
        return is

-- | Retrieve 'Issues' of a 'Project' assigned to the user.
getMyIssues :: ProjectId -> Redmine [Issue]
getMyIssues projectID       = getAllIssues
        [ ("project_id", BC.pack $ show projectID)
        , ("assigned_to_id", "me")
        ]

-- | Retrieve all 'Issues' of a 'Project'
getProjectsIssues :: ProjectId -> IssueFilter -> Redmine [Issue]
getProjectsIssues pID f     = do
        Issues is <- getEndPoint (GetProjectsIssues pID) $
                            [ ("offset", "0")
                            , ("limit", "100")
                            ] ++ f
        return is

-- | Retrieve an 'Issue'.
getIssue :: IssueId -> Redmine Issue
getIssue issueID            = getEndPoint (GetIssue issueID) []

-- | Update an 'Issue'. Currently, you must manually create the request's
-- JSON object.
updateIssue ::  IssueId -> LB.ByteString -> Redmine ()
updateIssue issueID         = putEndPoint $ UpdateIssue issueID


-- Statuses
-- | Retrieve all available statuses.
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
getCurrentUser              = getEndPoint GetCurrentUser []


-- Watching
-- | Add a watcher to an 'Issue'.
addWatcher :: IssueId -> User -> Redmine ()
addWatcher i user           = postEndPoint (AddWatcher i) postData
        where postData      = encode $ object [ "user_id" .= userId user ]

-- | Remove a watcher from an 'Issue'.
removeWatcher :: IssueId -> User -> Redmine ()
removeWatcher i user        = deleteEndPoint $ RemoveWatcher i $ userId user


-- Versions
-- | Retrieve all 'Versions' of a 'Project' from the 'projectId'.
getVersions :: ProjectId -> Redmine [Version]
getVersions p               = do Versions vs <- getEndPoint (GetVersions p) []
                                 return vs

-- | Retrieve a 'Version' from it's id.
getVersion :: VersionId -> Redmine Version
getVersion v                = getEndPoint (GetVersion v) []

-- | Retrieve all 'Issues' of a 'Version'.
getVersionsIssues :: ProjectId -> Version -> Redmine [Issue]
getVersionsIssues pID v     = getProjectsIssues pID
        [ ("fixed_version_id", BC.pack . show $ versionId v) ]

-- | Retrieve the next open 'Version' of a 'Project' with the soonest
-- 'versionDueDate'.
getNextVersionDue :: ProjectId -> Redmine (Maybe Version)
getNextVersionDue p         = do
        vs  <- getVersions p
        return . headMay . L.sortBy (compare `on` versionDueDate)
               . filter ((== "open") . versionStatus)
               . filter (isJust . versionDueDate) $ vs
