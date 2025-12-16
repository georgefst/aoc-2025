module Puzzles.Day3 (puzzle) where

import Pre

import Data.List.NonEmpty qualified as NE

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 3
        , parser = const $ (Bank <$> some1 digit) `sepEndBy` newline
        , parts =
            ( sum
                . map (digitsToInt . fromMaybe (error "battery list too short") . maxBatteries 2)
            )
                /\ ( sum
                        . map (digitsToInt . fromMaybe (error "battery list too short") . maxBatteries 12)
                   )
                /\ nil
        , extraTests = mempty
        }

newtype Bank = Bank (NonEmpty Battery)
    deriving newtype (Eq, Ord, Show)

newtype Battery = Battery Word8
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- maximal n-digit subsequence
-- returns `Nothing` if list isn't long enough (>= n)
maxBatteries :: Int -> Bank -> Maybe [Battery]
maxBatteries n0 (Bank bs0) = flip unfoldrM (n0, toList bs0) \case
    (0, _) -> pure Nothing
    (n, bs) -> do
        (b, i) <- findMax <$> nonEmpty (dropEnd (n - 1) bs)
        pure $ Just (b, (n - 1, drop (i + 1) bs))

-- returns the leftmost element in case of a tie
findMax :: (Ord a) => NonEmpty a -> (a, Int)
findMax = foldl1' (\m x -> if fst x > fst m then x else m) . flip NE.zip (0 :| [1 ..])
