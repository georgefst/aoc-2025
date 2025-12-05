module Puzzles.Day5 (puzzle) where

import Control.Monad
import Data.Functor
import Data.List.Extra
import Data.Maybe
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
            ]
        , extraTests = mempty
        }

data Range = Range
    { lower :: Int
    , upper :: Int
    }
    deriving (Eq, Ord, Show)

isInRange :: Int -> Range -> Bool
isInRange n r = n >= r.lower && n <= r.upper
