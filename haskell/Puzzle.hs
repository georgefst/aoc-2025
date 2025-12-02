module Puzzle where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

data Puzzle = forall input. Puzzle
    { number :: Word
    , parser :: Parsec Void Text input
    , parts :: [input -> Text]
    }
