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


-}
module Web.HTTP.Redmine
        (
        -- * Redmine Monad
          Redmine
        , runRedmine
        , defaultRedmineConfig
        -- * Redmine Types
        , Project(..)
        , Projects(..)
        , Issue(..)
        , Issues(..)
        -- ** API-related Types
        , RedmineConfig(..)
        -- * Redmine API Functions
        , getProjects
        , getAllIssues
        , getMyIssues
        -- * Formatting
        , projectsTable
        , projectDetail
        , issuesTable
        ) where

import qualified Data.ByteString.Char8 as BC    (pack)

import Web.HTTP.Redmine.Client
import Web.HTTP.Redmine.Format
import Web.HTTP.Redmine.Types


-- | Retrieve All the Projects
getProjects :: Redmine Projects
getProjects             = getEndPoint GetProjects []

-- | Retrieve All Issues of a Project
getAllIssues :: Integer -> Redmine Issues
getAllIssues projectID  = getEndPoint GetIssues
        [ ("project_id", BC.pack $ show projectID)
        ]

-- | Retrieve Issues of a Project Assigned to the User
getMyIssues :: Integer -> Redmine Issues
getMyIssues projectID   = getEndPoint GetIssues
        [ ("project_id", BC.pack $ show projectID)
        , ("assigned_to_id", "me")
        , ("offset", "0")
        , ("limit", "100")
        ]
