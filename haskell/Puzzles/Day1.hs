module Puzzles.Day1 (puzzle) where

import Pre
import Debug.Pretty.Simple

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 1
        , parser = const $ ((,) <$> ((char 'L' $> L) <|> (char 'R' $> R)) <*> (Inc <$> decimal)) `sepEndBy` newline
        , parts =
            ( sum
                . ( flip evalState 50
                        . traverse \(d, i) -> do
                            modify $ snd . step i d
                            p' <- get
                            pure $ Count if p' == 0 then 1 else 0
                  )
            )
                /\ ( sum
                        . flip evalState 50
                        . traverse \(d, i) -> do
                            p <- get
                            c <- state $ step i d
                            p' <- get
                            pure $ applyWhen (p == 0 && p' == 0) (pTraceShow "hmmmm") case d of
                                R -> abs c
                                L ->
                                    if
                                        | p == 0 -> abs c - 1
                                        | p' == 0 -> abs c + 1
                                        | otherwise -> abs c
                   )
                /\ nil
        , extraTests = mempty
        }

data Direction = L | R
    deriving (Eq, Ord, Show, Generic, NFData)

newtype Pos = Pos Int
    deriving newtype (Eq, Ord, Show, Num, NFData)

newtype Inc = Inc Int
    deriving newtype (Eq, Ord, Show, Num, NFData)

newtype Count = Count Int
    deriving newtype (Eq, Ord, Show, Num, NFData)

step :: Inc -> Direction -> Pos -> (Count, Pos)
step (Inc i) d (Pos p) = bimap Count Pos case d of
    L -> (p - i) `divMod` 100
    R -> (p + i) `divMod` 100
