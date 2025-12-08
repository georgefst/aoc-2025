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
        , parser = (V3 <$> decimal <* single ',' <*> decimal <* single ',' <*> decimal) `sepEndBy` newline
        , parts =
            [ TL.show
                . product
                . take 3
                . sortOn Down
                . map length
                . DS.toLists
                . snd
                . \boxes ->
                    -- TODO more principled way of distinguishing example and real
                    (!! ((if length boxes == 20 then 10 else if length boxes == 1000 then 1000 else undefined))) $
                        connectBoxes boxes
            ]
        , extraTests = mempty
        }

connectBoxes :: [V3 Int] -> [((V3 Int, V3 Int), DS.DisjointSet (V3 Int))]
connectBoxes boxes = zip allPairs $ scanl (flip $ uncurry DS.union) (foldMap DS.singleton boxes) allPairs
  where
    allPairs = sortOn (quadrance . uncurry (-)) $ filter (uncurry (/=)) $ allUnorderedPairs boxes

allUnorderedPairs :: [a] -> [(a, a)]
allUnorderedPairs = concat . join (zipWith (flip $ map . (,)) . tails)
