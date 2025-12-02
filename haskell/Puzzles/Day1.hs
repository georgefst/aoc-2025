module Puzzles.Day1 (puzzle1) where

import Control.Monad.State
import Data.Bifunctor
import Data.Functor
import Data.Text qualified as T
import Puzzle
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

puzzle1 :: Puzzle
puzzle1 =
    Puzzle
        { number = 1
        , parser = flip sepEndBy newline $ (,) <$> ((char 'L' $> L) <|> (char 'R' $> R)) <*> (Inc <$> Lex.decimal)
        , parts =
            [ T.show
                . sum
                . flip evalState 50
                . traverse \(d, i) -> state \p ->
                    let (_, p') = step i d p
                     in (Count if p' == 0 then 1 else 0, p')
            , T.show
                . sum
                . flip evalState 50
                . traverse \(d, i) -> state \p ->
                    let (c, p') = step i d p
                        c' = case d of
                            R -> abs c
                            L ->
                                if
                                    | p == 0 -> abs c - 1
                                    | p' == 0 -> abs c + 1
                                    | otherwise -> abs c
                     in (c', p')
            ]
        }

data Direction = L | R
    deriving (Eq, Ord, Show)

newtype Pos = Pos Int
    deriving newtype (Eq, Ord, Show, Num)

newtype Inc = Inc Int
    deriving newtype (Eq, Ord, Show, Num)

newtype Count = Count Int
    deriving newtype (Eq, Ord, Show, Num)

step :: Inc -> Direction -> Pos -> (Count, Pos)
step (Inc i) d (Pos p) = bimap Count Pos case d of
    L -> (p - i) `divMod` 100
    R -> (p + i) `divMod` 100
