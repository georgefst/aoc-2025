module Puzzles.Day8 (puzzle) where

import Pre

import Data.DisjointSet qualified as DS
import Data.Text.Lazy qualified as TL
import Linear.Metric
import Linear.V3

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 8
        , parser = \isRealData -> (if isRealData then 1000 else 10,) <$> (V3 <$> decimal <* single ',' <*> decimal <* single ',' <*> decimal) `sepEndBy` newline
        , parts =
            [ uncurry \n -> TL.show
                . product
                . take 3
                . sortOn Down
                . map length
                . DS.toLists
                . snd
                . (!! n)
                . connectBoxes
            , uncurry . const $ TL.show
                . maybe (error "sets never unified") (\((V3 x1 _ _, V3 x2 _ _), _) -> x1 * x2)
                . lastMay
                . takeWhile ((> 1) . DS.sets . snd)
                . connectBoxes
            ]
        , extraTests = mempty
        }

connectBoxes :: [V3 Int] -> [((V3 Int, V3 Int), DS.DisjointSet (V3 Int))]
connectBoxes boxes = zip allPairs $ scanl (flip $ uncurry DS.union) (foldMap DS.singleton boxes) allPairs
  where
    allPairs = sortOn (quadrance . uncurry (-)) $ filter (uncurry (/=)) $ allUnorderedPairs boxes

allUnorderedPairs :: [a] -> [(a, a)]
allUnorderedPairs = concat . join (zipWith (flip $ map . (,)) . tails)
