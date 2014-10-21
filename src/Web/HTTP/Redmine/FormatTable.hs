{-|
-
- This Module contains functions and types for drawing tables from lists.
-
- It was originally taken from this StackOverflow post, but has been
- modified heavily:
- https://stackoverflow.com/questions/5929377/format-list-output-in-haskell
-
-}
module Web.HTTP.Redmine.FormatTable
        ( Alignment(..)
        , HorizontalAlignment(..)
        , VerticalAlignment(..)
        , Column(..)
        , formatTable
        ) where

import Prelude hiding   (Either(..))

import Control.Monad    (join)
import Data.List        (elemIndex, transpose, intercalate)
import Data.Maybe       (fromJust, isJust)

-- | The horizontal and vertical alignment of a single table cell.
data Alignment      = Alignment
                    { verticalAlign     :: VerticalAlignment
                    , horizontalAlign   :: HorizontalAlignment
                    } deriving (Show)
-- | Vertical Alignment controls the height padding within a cell.
data VerticalAlignment = Top | Middle | Bottom deriving (Show, Eq)
-- | Horizontal alignment controls the width padding within a cell.
data HorizontalAlignment = Left | Center | Right deriving (Show, Eq)

-- | A Column descriptor
data Column a       = Column
                    { headerName        :: String
                    , headerAlign       :: Alignment
                    , dataAlign         :: Alignment
                    , columnWeight      :: Weight
                    , dataSelector      :: a -> String
                    }

-- | The width weight of a column, relative to all other columns. Columns
-- with higher weights get more characters per line.
type Weight         = Integer
-- | The Height of a Table Row or Column.
type Height         = Integer
-- | The Width of a Table Row or Column.
type Width          = Integer

-- | Create a table from a list of columns, items and a width.
formatTable :: [Column a] -> Width -> [a] -> String
formatTable cs w is = intercalate ("\n" ++ sep ws ++ "\n") $ heads:rows
        where sep   = intercalate "-+-" . map ((`replicate` '-') . fromIntegral)
              heads = formatHeader cs ws
              rows  = map (formatRow cs ws) is
              ws    = shrinkToMaxRowLengths cs is $ makeWeights cs is w


-- Width Generation

-- | Make a list of column Widths by starting from the minimums and
-- increasing widths in search of the ideal width ratio defined by the
-- column weights.
makeWeights :: [Column a] -> [a] -> Width -> [Width]
makeWeights cs is w         = build minimumWidths
        where minimumWidths = map (minimumWidth is) cs
              targetWeight  = w - toInteger (3 * (length cs - 1))
              ratios ws     = map ((/ toRational (sum ws)) . toRational) ws
              weights       = map columnWeight cs
              build current = if sum current < targetWeight && isJust bumpIndex
                              then build $ take (fromJust bumpIndex) current ++
                                   current !! fromJust bumpIndex + 1 :
                                   drop (fromJust bumpIndex + 1) current
                              else current
                where bumpIndex  = elemIndex (maximum ratioDiffs) ratioDiffs
                      ratioDiffs = zipWith (-) (ratios weights) (ratios current)

-- | Reduce a list of column widths to the length of the column's longest
-- row.
shrinkToMaxRowLengths :: [Column a] -> [a] -> [Width] -> [Width]
shrinkToMaxRowLengths cs is     = zipWith shrinkColumn cs
        where shrinkColumn c w  = min w . maximum . map (toInteger . length) .
                                  concatMap (wrap w) $ getColumnContents c is

-- | Determine the minimum width of a column by finding it's longest word.
minimumWidth :: [a] -> Column a -> Width
minimumWidth is c   = maximum . concatMap (map (fromIntegral . length) . words)
                    $ getColumnContents c is

-- | Get all the data in a column, including the header.
getColumnContents :: Column a -> [a] -> [String]
getColumnContents c is  = headerName c : map (dataSelector c) is


-- Row Formatting

-- | Create a Header row from a list of Columns and Widths
formatHeader :: [Column a] -> [Width] -> String
formatHeader cs ws      = intercalate " | " . join
                        . zipWith3 (align 1) ws (map headerAlign cs)
                        $ map (\c -> [headerName c]) cs

-- | Create a table row from a list of Columns and Widths
formatRow :: [Column a] -> [Width] -> a -> String
formatRow cs ws i       = intercalate "\n" . map (intercalate " | ") . transpose
                        $ final
        where final     = zipWith3 (align rowHeight) ws (map dataAlign cs) wrapped
              rowHeight = toInteger . maximum $ map length wrapped
              wrapped   = zipWith wrap ws values
              values    = map (`dataSelector` i) cs


-- Line Wrapping

-- | Wrap a string into multiple lines by words given a maximum width.
wrap :: Width -> String -> [String]
wrap width              = wrapLine width [] "" . words

-- | Recursively wrap a line by words, using the width, current result, current
-- word and a list of words to process.
wrapLine :: Width -> [String] -> String -> [String] -> [String]
wrapLine _ result c []      = result ++ [c]
wrapLine w result c (i:is)
        | newLength > w &&
          null c            = wrapLine w result i is
        | newLength > w     = wrapLine w (result ++ [c]) i is
        | newLength <= w &&
          c == ""           = wrapLine w result i is
        | otherwise         = wrapLine w result (c ++ " " ++ i) is
        where  newLength    = toInteger . length $ c ++ " " ++ i


-- Alignment

-- | Align a wrapped string horizantally and vertically.
align :: Height -> Width -> Alignment -> [String] -> [String]
align h w a         = map (alignHorizontally w . horizontalAlign $ a) .
                     (alignVertically h . verticalAlign $ a)

-- | Horizaontally align a String, given a HorizontalAlignment and a Width.
alignHorizontally :: Width -> HorizontalAlignment -> String -> String
alignHorizontally w h s
        | h == Left     = s ++ replicate x ' '
        | h == Right    = replicate x ' ' ++ s
        | otherwise     = replicate l ' ' ++ s ++ replicate r ' '
        where x         =  fromIntegral w - length s
              l         = x `div` 2
              r         = x - l

-- | Verticlaly align a String, given a VerticalAlignment and a Height
alignVertically :: Height -> VerticalAlignment -> [String] -> [String]
alignVertically h v ss
        | v == Top      = ss ++ replicate x ""
        | v == Bottom   = replicate x "" ++ ss
        | otherwise     = replicate t "" ++ ss ++ replicate b ""
          where x       = fromIntegral h - length ss
                t       = x `div` 2
                b       = x - t
