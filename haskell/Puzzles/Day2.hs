module Puzzles.Day2 (puzzle) where

import Pre

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 2
        , parser = const $ (<* newline) $ ((,) <$> (decimal <* char '-') <*> decimal) `sepBy` (char ',')
        , parts =
            [ TL.show
                . sum
                . concatMap
                    (mapMaybe (\n -> guard (isRepetition2 n) $> n) . uncurry enumFromTo)
            , TL.show
                . sum
                . concatMap
                    (mapMaybe (\n -> guard (isRepetitionN n) $> n) . uncurry enumFromTo)
            ]
        , extraTests = mempty
        }

newtype ID = ID Int
    deriving newtype (Eq, Ord, Show, Num, Enum)

isRepetition2 :: ID -> Bool
isRepetition2 (T.show -> n) = case T.length n `divMod` 2 of
    (d, 0) -> equalChunks n d
    _ -> False

isRepetitionN :: ID -> Bool
isRepetitionN (T.show -> n) = flip any [1 .. T.length n `div` 2] $ equalChunks n

equalChunks :: Text -> Int -> Bool
equalChunks n i = case T.chunksOf i n of
    [] -> True
    x : xs -> all (== x) xs
