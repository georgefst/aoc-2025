module Puzzles.Day6 (puzzle) where

import Pre

import Data.Text.Lazy qualified as TL

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 6
        , parser = do
            ints <- (hspace *> (decimal `sepBy1` hspace1)) `sepEndBy1` newline
            ops <- ((single '*' $> Multiply) <|> (single '+' $> Add)) `sepEndBy` hspace1
            void newline
            pure (ops, transpose ints)
        , parts =
            [ TL.show
                . sum
                . uncurry (zipWith \op -> foldl' (apply op) (unit op))
            ]
        , extraTests = mempty
        }

data Op = Add | Multiply
    deriving (Eq, Ord, Show, Enum, Bounded)
apply :: Op -> Int -> Int -> Int
apply = \case
    Add -> (+)
    Multiply -> (*)
unit :: Op -> Int
unit = \case
    Add -> 0
    Multiply -> 1
