module Puzzles.Day9 (puzzle) where

import Pre

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 9
        , parser = const $ (V2 <$> decimal <* single ',' <*> decimal) `sepEndBy1` newline
        , parts =
            [ maximum
                . fmap (squareSize . uncurry Square)
                . fromMaybe (error "input too small")
                . nonEmpty
                . allUnorderedPairs False
            , \points ->
                let path =
                        fromMaybe (error "malformed line")
                            . traverse mkLine
                            $ (last points', head points') :| adjacentPairs points
                      where
                        points' = fromMaybe (error "empty input") $ nonEmpty points
                 in snd
                        . fromMaybe (error "no solutions")
                        . find (not . flip any path . lineIntersectsSquare . fst)
                        . sortOn (Down . snd)
                        . fmap ((id &&& squareSize) . uncurry Square)
                        $ allUnorderedPairs False points
            ]
        , extraTests = mempty
        }

data Square = Square
    { corner1 :: V2 Int
    , corner2 :: V2 Int
    }
    deriving (Show)
squareSize :: Square -> Int
squareSize Square{corner1, corner2} = (\(V2 x y) -> x * y) . (+ 1) . fmap abs $ corner1 - corner2

data Line
    = LineHorizontal {y :: Int, x1 :: Int, x2 :: Int}
    | LineVertical {x :: Int, y1 :: Int, y2 :: Int}
    deriving (Show)
mkLine :: (V2 Int, V2 Int) -> Maybe Line
mkLine (V2 x1 y1, V2 x2 y2)
    | y1 == y2 = Just $ LineHorizontal{y = y1, x1, x2}
    | x1 == x2 = Just $ LineVertical{x = x1, y1, y2}
    | otherwise = Nothing

data Interval = Interval Int Int deriving (Show)
compareToInterval :: Int -> Interval -> Ordering
compareToInterval n (Interval l u)
    | n <= l = LT
    | n >= u = GT
    | otherwise = EQ

squareIntervals :: Square -> V2 Interval
squareIntervals Square{corner1, corner2} = uncurry Interval . sortPair <$> liftA2 (,) corner1 corner2

lineIntersectsSquare :: Square -> Line -> Bool
lineIntersectsSquare (squareIntervals -> V2 intervalX intervalY) = \case
    LineHorizontal{y, x1, x2} ->
        compareToInterval y intervalY == EQ
            && compareToInterval x1 intervalX /= compareToInterval x2 intervalX
    LineVertical{x, y1, y2} ->
        compareToInterval x intervalX == EQ
            && compareToInterval y1 intervalY /= compareToInterval y2 intervalY
