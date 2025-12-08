module Puzzles.Day1 (puzzle) where

import Pre

import Data.Text.Lazy qualified as TL

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 1
        , parser = const $ flip sepEndBy newline $ (,) <$> ((char 'L' $> L) <|> (char 'R' $> R)) <*> (Inc <$> decimal)
        , parts =
            [ TL.show
                . sum
                . flip evalState 50
                . traverse \(d, i) -> do
                    modify $ snd . step i d
                    p' <- get
                    pure $ Count if p' == 0 then 1 else 0
            , TL.show
                . sum
                . flip evalState 50
                . traverse \(d, i) -> do
                    p <- get
                    c <- state $ step i d
                    p' <- get
                    pure case d of
                        R -> abs c
                        L ->
                            if
                                | p == 0 -> abs c - 1
                                | p' == 0 -> abs c + 1
                                | otherwise -> abs c
            ]
        , extraTests = mempty
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
