module Puzzles.Day3 (puzzle) where

import Control.Monad.Loops (unfoldrM)
import Data.Char (digitToInt)
import Data.Foldable1
import Data.List.Extra (dropEnd)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, some1)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text.Lazy qualified as TL
import Data.Word
import Puzzle
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, newline)

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 3
        , parser = flip sepEndBy newline $ Bank . fmap (fromIntegral . digitToInt) <$> some1 digitChar
        , parts =
            [ TL.show
                . sum
                . map (digitsToInt . fromMaybe (error "battery list too short") . maxBatteries 2)
            , TL.show
                . sum
                . map (digitsToInt . fromMaybe (error "battery list too short") . maxBatteries 12)
            ]
        , extraTests = mempty
        }

newtype Bank = Bank (NonEmpty Battery)
    deriving newtype (Eq, Ord, Show)

newtype Battery = Battery Word8
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- maximal n-digit subsequence
-- returns `Nothing` if list isn't long enough (>= n)
maxBatteries :: Int -> Bank -> Maybe [Battery]
maxBatteries n0 (Bank bs0) = flip unfoldrM (n0, NE.toList bs0) \case
    (0, _) -> pure Nothing
    (n, bs) -> do
        (b, i) <- findMax <$> nonEmpty (dropEnd (n - 1) bs)
        pure $ Just (b, (n - 1, drop (i + 1) bs))

-- returns the leftmost element in case of a tie
findMax :: (Ord a) => NonEmpty a -> (a, Int)
findMax = foldl1' (\m x -> if fst x > fst m then x else m) . flip NE.zip (0 :| [1 ..])

digitsToInt :: [Battery] -> Int
digitsToInt = snd . foldr (\b (p, acc) -> (10 * p, acc + fromIntegral b * p)) (1, 0)
