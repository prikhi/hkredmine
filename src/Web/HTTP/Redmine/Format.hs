{-|
-
- This Module contains functions for prettifying 'Redmine Types' for
- display.
-
-}
module Web.HTTP.Redmine.Format
        ( projectsTable
        , projectDetail
        , issuesTable
        ) where

import Data.List    (intercalate)

import Web.HTTP.Redmine.FormatTable
import Web.HTTP.Redmine.Types

-- | Create a Table of Projects with ID, Name, Identifier, Description
-- & Updated Columns
projectsTable :: [Project] -> String
projectsTable = showTable [ ColDesc center "ID" left (show . projectId)
                          , ColDesc center "Name" left projectName
                          , ColDesc center "Identifier" left projectIdentifier
                          , ColDesc center "Description" left (takeWhile (/= '\r')  . projectDescription)
                          , ColDesc center "Updated" left (takeWhile (/= 'T') . projectUpdated)
                          ]

-- | Create a String Displaying a Project's Name, ID, Identifier,
-- Created/Updated Dates & Description
projectDetail :: Project -> String
projectDetail p = intercalate "\n"
        [ projectName p
        , replicate (length (projectName p) + 1) '-'
        , ""
        , "ID: " ++ show (projectId p)
        , "Identifier: " ++ projectIdentifier p
        , ""
        , "Created On: " ++ projectCreated p
        , "Updated On: " ++ projectUpdated p
        , ""
        , "Description:"
        , projectDescription p
        ]

-- | Create a Table of Issues with an ID#, Tracker, Status, Priority,
-- Category & Subject.
issuesTable :: [Issue] -> String
issuesTable = showTable [ ColDesc center "ID" left (show . issueId)
                        , ColDesc center "Subject" left issueSubject
                        , ColDesc center "Status" left issueStatus
                        , ColDesc center "Priority" left issuePriority
                        , ColDesc center "Tracker" left issueTracker
                        , ColDesc center "Category" left issueCategory
                        ]

