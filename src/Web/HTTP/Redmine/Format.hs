{-|
-
- This Module contains functions for prettifying 'Redmine Types' for
- display.
-
-}
module Web.HTTP.Redmine.Format
        ( projectsTable
        , issuesTable
        , versionTable
        , projectDetail
        , issueDetail
        , versionDetail
        ) where

import Prelude hiding (Either(..))

import Data.List    (intercalate)
import Data.Maybe   (fromMaybe, fromJust, isJust)

import Web.HTTP.Redmine.FormatTable
import Web.HTTP.Redmine.Types

-- | Create a Table of Projects with ID, Name, Identifier, Description
-- & Updated Columns
projectsTable :: Integer -> [Project] -> String
projectsTable = formatTable
        [ Column "ID" middleCenter middleLeft 5 $ show . projectId
        , Column "Identifier" middleCenter middleCenter 22 projectIdentifier
        , Column "Name" middleCenter middleLeft 17 projectName
        , Column "Description" middleCenter middleLeft 35 $
                takeWhile (/= '\r') . projectDescription
        , Column "Updated" middleCenter middleCenter 12 $
                takeWhile (/= 'T') . projectUpdated
        ]

middleCenter, middleLeft :: Alignment
middleCenter    = Alignment Middle Center
middleLeft      = Alignment Middle Left

-- | Create a Table of Issues with an ID#, Tracker, Status, Priority,
-- Category & Subject.
issuesTable :: Integer -> [Issue] -> String
issuesTable = formatTable
        [ Column "ID" middleCenter middleLeft 7 $ show . issueId
        , Column "Tracker" middleCenter middleCenter 15 issueTracker
        , Column "Priority" middleCenter middleCenter 13 issuePriority
        , Column "Subject" middleCenter middleLeft 50 issueSubject
        , Column "Status" middleCenter middleCenter 15 issueStatus
        , Column "Category" middleCenter middleCenter 17 $
                fromMaybe "" . issueCategory
        ]

-- | Create a Table of Versions.
versionTable :: Integer -> [Version] -> String
versionTable = formatTable
        [ Column "ID" middleCenter middleLeft 5 $ show . versionId
        , Column "Name" middleCenter middleCenter 20 versionName
        , Column "Status" middleCenter middleCenter 12 versionStatus
        , Column "Description" middleCenter middleLeft 25 $
                takeWhile (/= '\r') . versionDescription
        , Column "Due Date" middleCenter middleCenter 15 $
                fromMaybe "" . versionDueDate
        ]


-- | Create a String Displaying a Project's Name, ID, Identifier,
-- Created/Updated Dates & Description
projectDetail :: Project -> String
projectDetail p = intercalate "\n"
        [ projectName p
        , horizLine '=' $ projectName p
        , ""
        , "Identifier:  " ++ projectIdentifier p
        , ""
        , "Created On:  " ++ projectCreated p
        , "Updated On:  " ++ projectUpdated p
        , ""
        , "Description:"
        , projectDescription p
        ]

-- | Create a Tring displaying the details of a single 'Issue'.
issueDetail :: Issue -> String
issueDetail i       = intercalate "\n" $
        [ name
        , horizLine '=' name
        , "Created By:  " ++ issueAuthor i
        , ""
        , "Status:      " ++ issueStatus i
        , "Priority:    " ++ issuePriority i ] ++
        [ "Assignee:    " ++ fromJust (issueAssignedTo i)
                | isJust (issueAssignedTo i) ] ++
        [ "" ] ++
        [ "Category:    " ++ fromJust (issueCategory i)
                | isJust (issueCategory i) ] ++
        [ "Version:     " ++ fromJust (issueVersion i)
                | isJust (issueVersion i) ] ++
        [ ""    | isJust (issueDueDate i) || isJust (issueStartDate i) ] ++
        [ "Start Date:  " ++ fromJust (issueStartDate i)
                | isJust (issueStartDate i) ] ++
        [ "Stop Date:   " ++ fromJust (issueDueDate i)
                | isJust (issueDueDate i) ] ++
        [ show (issueDoneRatio i) ++ "% Done" ] ++
        [ "\nDescription:\n" ++ issueDescription i
                | issueDescription i /= "" ]
        where name  = issueTracker i ++ " #" ++ show (issueId i) ++ ": " ++
                      issueSubject i

-- | Create a string to desiplay the Id, Name, Description and Due Date of
-- a 'Version'.
versionDetail :: Version -> String
versionDetail v     = intercalate "\n" $
        [ vn
        , horizLine '=' vn
        , ""
        , "ID:      " ++ show (versionId v)
        , "Status:  " ++ versionStatus v
        ] ++
        [ "Due:     " ++ fromJust (versionDueDate v)
                | isJust (versionDueDate v) ] ++
        [ "\nDescription:\n" ++ vd
                | vd /= "" ]
        where vn    = versionName v
              vd    = versionDescription v

-- | Create a horizontal line the length of another string(+1)
horizLine :: Char -> String -> String
horizLine c s = replicate (length s + 1) c
