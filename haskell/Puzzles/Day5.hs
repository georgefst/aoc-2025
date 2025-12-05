module Puzzles.Day5 (puzzle) where

import Control.Monad
import Data.List
import Data.Ord
import Data.Text.Lazy qualified as TL
import Puzzle
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 5
        , parser = do
            ranges <- flip sepEndBy newline $ Range <$> Lex.decimal <* single '-' <*> Lex.decimal
            void newline
            vals <- sepEndBy Lex.decimal newline
            pure (ranges, vals)
        , parts =
            [ \(ranges, vals) ->
                TL.show
                    . length
                    . filter (flip any ranges . isInRange)
                    $ vals
            , TL.show
                . sum
                . map rangeLength
                . foldr addInterval []
                . sortOn (Down . (.lower))
                . fst
            ]
        , extraTests = mempty
        }

data Range = Range
    { lower :: Int
    , upper :: Int
    }
    deriving (Eq, Ord, Show)

rangeLength :: Range -> Int
rangeLength r = r.upper - r.lower + 1

isInRange :: Int -> Range -> Bool
isInRange n r = n >= r.lower && n <= r.upper

extend :: Int -> Range -> Range
extend upper r = r{upper = max r.upper upper}

addInterval :: Range -> [Range] -> [Range]
addInterval new = \case
    [] -> [new]
    (r : rs) ->
        if isInRange new.lower r
            then extend new.upper r : rs
            else new : r : rs
