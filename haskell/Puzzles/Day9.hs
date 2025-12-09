module Puzzles.Day9 (puzzle) where

import Pre

import Data.Text.Lazy qualified as TL

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 9
        , parser = const $ (V2 @Int <$> decimal <* single ',' <*> decimal) `sepEndBy1` newline
        , parts =
            [ TL.show
                . maximum
                . fmap ((\(V2 x y) -> x * y) . (+ 1) . fmap abs . uncurry (-))
                . fromMaybe (error "empty input")
                . nonEmpty
                . allUnorderedPairs False
            ]
        , extraTests = mempty
        }
