module Main (main) where

import Control.Monad.State
import Data.Bifunctor
import Data.Traversable
import Text.Read (readMaybe)

main :: IO ()
main = do
    Just input <-
        traverse
            ( \case
                'L' : (readMaybe -> Just i) -> Just (L, Inc i)
                'R' : (readMaybe -> Just i) -> Just (R, Inc i)
                _ -> Nothing
            )
            . lines
            <$> readFile "inputs/examples/1"
    print
        . sum
        . flip evalState 50
        $ for input \(d, i) -> state \p ->
            let (_, p') = step i d p
             in (Count if p' == 0 then 1 else 0, p')
    print
        . sum
        . flip evalState 50
        $ for input \(d, i) -> state \p ->
            let (c, p') = step i d p
                c' = case d of
                    R -> abs c
                    L ->
                        if
                            | p == 0 -> abs c - 1
                            | p' == 0 -> abs c + 1
                            | otherwise -> abs c
             in (c', p')

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
