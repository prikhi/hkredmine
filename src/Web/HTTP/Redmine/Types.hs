{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
-
- This Module contains the Redmine-specific Types and their JSON parsing
- instances.
-
-}
module Web.HTTP.Redmine.Types
        ( EndPoint(..)
        , Project(..)
        , Projects(..)
        , Issue(..)
        , Issues(..)
        ) where

import Control.Applicative  ((<$>), (<*>))
import Data.Aeson           ((.:), FromJSON, parseJSON, Value(Object))


-- | API EndPoints
data EndPoint    = GetProjects
                 | GetIssues
                 deriving (Show)

-- | A Redmine Project
data Project = Project { projectId          :: Integer  -- ^ The Project's ID Number
                       , projectName        :: String   -- ^ The Project's Name
                       , projectIdentifier  :: String   -- ^ The Project's Identifier
                       , projectDescription :: String   -- ^ The Project's Description
                       , projectCreated     :: String   -- ^ The Date the Project Was Created
                       , projectUpdated     :: String   -- ^ The Date the Project Was Last Updated
                       } deriving (Show)

-- | A List of Redmine Projects
newtype Projects = Projects [Project] deriving (Show)

-- | A Redmine Issue
data Issue = Issue { issueId :: Integer
                   , issueTracker :: String
                   , issueStatus :: String
                   , issuePriority :: String
                   , issueAuthor :: String
                   , issueAssignedTo :: String
                   , issueCategory :: String
                   , issueVersion :: String
                   , issueSubject :: String
                   , issueDescription :: String
                   , issueDoneRatio :: Integer
                   , issueCreated :: String
                   , issueUpdated :: String
                   } deriving (Show)

-- | A List of Redmine Issues
newtype Issues = Issues [Issue] deriving (Show)

-- JSON Parsing Instances
instance FromJSON Project where
        parseJSON (Object v) =
                Project <$> v .: "id"
                        <*> v .: "name"
                        <*> v .: "identifier"
                        <*> v .: "description"
                        <*> v .: "created_on"
                        <*> v .: "updated_on"
        parseJSON _          = fail "Unable to parse Project JSON Object"

instance FromJSON Projects where
        parseJSON (Object v) = do
                projectArray <- v .: "projects"
                projectsList <- mapM parseJSON projectArray
                return $ Projects projectsList
        parseJSON _          = fail "Unable to parse Projects JSON Object"

instance FromJSON Issue where
        parseJSON (Object v) =
                Issue <$> v .: "id"
                      <*> v `grabName` "tracker"
                      <*> v `grabName` "status"
                      <*> v `grabName` "priority"
                      <*> v `grabName` "author"
                      <*> v `grabName` "assigned_to"
                      <*> v `grabName` "category"
                      <*> v `grabName` "fixed_version"
                      <*> v .: "subject"
                      <*> v .: "description"
                      <*> v .: "done_ratio"
                      <*> v .: "created_on"
                      <*> v .: "updated_on"
                where object `grabName` attr = object .: attr >>=
                                               \(Object o) -> o .: "name"
        parseJSON _          = fail "Unable to parse Issue JSON Object"

instance FromJSON Issues where
        parseJSON (Object v) = do
                issueArray <- v .: "issues"
                issuesList <- mapM parseJSON issueArray
                return $ Issues issuesList
        parseJSON _          = fail "Unable to parse Issues JSON Object"
