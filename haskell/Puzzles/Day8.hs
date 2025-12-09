module Puzzles.Day8 (puzzle) where

import Pre

import Control.Lens
import Data.DisjointSet qualified as DS
import Data.Text.Lazy qualified as TL
import Linear.Metric
import Linear.V3

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 8
        , parser = \isRealData ->
            (if isRealData then 1000 else 10,)
                <$> (V3 <$> decimal <* single ',' <*> decimal <* single ',' <*> decimal) `sepEndBy` newline
        , parts =
            [ uncurry \n ->
                TL.show
                    . product
                    . take 3
                    . sortOn Down
                    . map length
                    . DS.toLists
                    . maybe (error "not enough boxes") snd
                    . listIndex n
                    . connectBoxes
            , uncurry . const $
                TL.show
                    . uncurry ((*) `on` view _x)
                    . maybe (error "sets never unified") fst
                    . lastMay
                    . takeWhile ((> 1) . DS.sets . snd)
                    . connectBoxes
            ]
        , extraTests = mempty
        }

connectBoxes :: [V3 Int] -> [((V3 Int, V3 Int), DS.DisjointSet (V3 Int))]
connectBoxes boxes = zip allPairs $ scanl (flip $ uncurry DS.union) (foldMap DS.singleton boxes) allPairs
  where
    allPairs = sortOn (quadrance . uncurry (-)) $ allUnorderedPairs False boxes
