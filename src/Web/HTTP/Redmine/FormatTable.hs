{-|
-
- This Module contains functions and types for drawing tables from lists.
-
- It was originally taken from this StackOverflow post:
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

import Data.List
import Control.Monad    (join)

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
formatTable cs w is = intercalate ("\n" ++ sep ++ "\n") $ heads:rows
        where ws    = map (\x -> x - 2) $ applyWidthWeights (map columnWeight cs) (w - 1)
              sep   = intercalate "-+-" . map (`replicate` '-') $ map fromIntegral ws
              heads = formatHeader cs ws
              rows  = map (formatRow cs ws) is

-- | Turn a list of weights and a width into a list of weighted widths.
applyWidthWeights :: [Weight] -> Width -> [Width]
applyWidthWeights weights width = map ((`div` totalWeight) . (* width)) weights
        where totalWeight       = sum weights

-- | Create a Header row from a list of Columns and Widths
formatHeader :: [Column a] -> [Width] -> String
formatHeader cs ws      = intercalate " | " . join
                        $ zipWith3 (align 1) ws (map headerAlign cs)
                        $ map (\c -> [headerName c]) cs

-- | Create a table row from a list of Columns and Widths
formatRow :: [Column a] -> [Width] -> a -> String
formatRow cs ws i   = intercalate "\n" $ map (intercalate " | ") $ transpose final
        where values    = map (`dataSelector` i) cs
              wrapped   = zipWith wrap ws values
              final     = zipWith3 (align rowHeight) ws (map dataAlign cs) wrapped
              rowHeight = toInteger . maximum $ map length wrapped

-- | Wrap a string into multiple lines by words given a maximum width.
wrap :: Width -> String -> [String]
wrap width  s       = wrapLine width [] "" $ words s

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
        where  newLength = toInteger . length $ c ++ " " ++ i

-- | Align a wrapped string horizantally and vertically.
align :: Height -> Width -> Alignment -> [String] -> [String]
align h w a ss      = map (alignHorizontally w . horizontalAlign $ a) $
                     (alignVertically h . verticalAlign $ a) ss

-- | Horizaontally align a String, given a HorizontalAlignment and a Width.
alignHorizontally :: Width -> HorizontalAlignment -> String -> String
alignHorizontally w h s
        | h == Left     = s ++ replicate x ' '
        | h == Right    = replicate x ' ' ++ s
        | otherwise     = replicate l ' ' ++ s ++ replicate r ' '
        where x =  fromIntegral w - length s
              l = x `div` 2
              r = x - l

-- | Verticlaly align a String, given a VerticalAlignment and a Height
alignVertically :: Height -> VerticalAlignment -> [String] -> [String]
alignVertically h v ss
        | v == Top      = ss ++ replicate x ""
        | v == Bottom   = replicate x "" ++ ss
        | otherwise     = replicate t "" ++ ss ++ replicate b ""
          where x = fromIntegral h - length ss
                t = x `div` 2
                b = x - t
