{-|
-
- This Module contains functions and types for drawing tables from lists.
-
- It is taken from this StackOverflow post:
- https://stackoverflow.com/questions/5929377/format-list-output-in-haskell
-
-}
module Web.HTTP.Redmine.FormatTable
        ( ColDesc(..)
        , left
        , right
        , center
        , showTable
        ) where


import Data.List

-- | A type for fill functions
type Filler         = Int -> String -> String

-- | A type for describing table columns
data ColDesc t      = ColDesc
        { colTitleFill :: Filler
        , colTitle     :: String
        , colValueFill :: Filler
        , colValue     :: t -> String
        }

-- | Functions that fill a string (s) to a given width (n) by adding pad
-- character (c) to align left, right, or center
fillLeft, fillRight, fillCenter :: Char -> Int -> String -> String
fillLeft c n s      = s ++ replicate (n - length s) c
fillRight c n s     = replicate (n - length s) c ++ s
fillCenter c n s    = replicate l c ++ s ++ replicate r c
        where x     = n - length s
              l     = x `div` 2
              r     = x - l

-- | Functions that fill with spaces
left, right, center :: Int -> String -> String
left                = fillLeft ' '
right               = fillRight ' '
center              = fillCenter ' '

-- Converts a list of items into a table according to a list of column
-- descriptors
showTable :: [ColDesc t] -> [t] -> String
showTable cs ts     =
    let header      = map colTitle cs
        rows        = [[colValue c t | c <- cs] | t <- ts]
        widths      = [maximum $ map length col | col <- transpose $ header : rows]
        separator   = intercalate "-+-" [replicate width '-' | width <- widths]
        fillCols fill cols = intercalate " | " [fill c width col | (c, width, col) <- zip3 cs widths cols]
    in  unlines $ fillCols colTitleFill header : separator : map (fillCols colValueFill) rows
