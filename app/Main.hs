module Main (main) where

import Control.Monad.State
import Data.Bifunctor
import Text.Read (readMaybe)

main :: IO ()
main = do
    runPuzzle puzzle1

runPuzzle :: Puzzle a -> IO ()
runPuzzle p = do
    Just input <- p.parse <$> readFile ("inputs/examples/" <> show p.number)
    putStrLn $ p.part1.solve input
    putStrLn $ p.part2.solve input
data Puzzle input = Puzzle
    { number :: Word
    , parse :: String -> Maybe input
    , part1 :: Part input
    , part2 :: Part input
    }
data Part input = Part
    { solve :: input -> String
    }

puzzle1 :: Puzzle [(Direction, Inc)]
puzzle1 =
    Puzzle
        { number = 1
        , parse =
            traverse
                ( \case
                    'L' : (readMaybe -> Just i) -> Just (L, Inc i)
                    'R' : (readMaybe -> Just i) -> Just (R, Inc i)
                    _ -> Nothing
                )
                . lines
        , part1 =
            Part
                { solve =
                    show
                        . sum
                        . flip evalState 50
                        . traverse \(d, i) -> state \p ->
                            let (_, p') = step i d p
                             in (Count if p' == 0 then 1 else 0, p')
                }
        , part2 =
            Part
                { solve =
                    show
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
                }
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
