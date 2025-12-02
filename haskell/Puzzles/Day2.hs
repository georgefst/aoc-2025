module Puzzles.Day2 (puzzle2) where

import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Puzzle
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

puzzle2 :: Puzzle
puzzle2 =
    Puzzle
        { number = 2
        , parser = (<* newline) $ flip sepBy (char ',') $ (,) <$> (Lex.decimal <* char '-') <*> Lex.decimal
        , parts =
            [ T.show
                . sum
                . concatMap
                    (mapMaybe (\n -> guard (isRepetition2 n) $> n) . uncurry enumFromTo)
            , T.show
                . sum
                . concatMap
                    (mapMaybe (\n -> guard (isRepetitionN n) $> n) . uncurry enumFromTo)
            ]
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
