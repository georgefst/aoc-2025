module Puzzle where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Void
import Test.Tasty
import Text.Megaparsec

data Puzzle = forall input. Puzzle
    { number :: Word
    , parser :: Parsec Void Text input
    , parts :: [input -> TL.Text]
    , extraTests :: Bool -> FilePath -> IO input -> [TestTree]
    }
