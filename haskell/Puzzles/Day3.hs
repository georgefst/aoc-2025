module Puzzles.Day3 (puzzle) where

import Data.Char (digitToInt)
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, some1)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
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
            [ T.show
                . sum
                . map (digitsToInt . fromMaybe (error "battery list too short") . maxBatteries)
            ]
        }

newtype Bank = Bank (NonEmpty Battery)
    deriving newtype (Eq, Ord, Show)

newtype Battery = Battery Word8
    deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

-- returns `Nothing` if list isn't long enough (>= 2)
maxBatteries :: Bank -> Maybe (Battery, Battery)
maxBatteries (Bank bs) = do
    (b1, i) <- findMax <$> nonEmpty (NE.init bs)
    bs' <- nonEmpty $ NE.drop (i + 1) bs
    let (b2, _) = findMax bs'
    pure (b1, b2)

-- returns the leftmost element in case of a tie
findMax :: (Ord a) => NonEmpty a -> (a, Int)
findMax = foldl1' (\m x -> if fst x > fst m then x else m) . flip NE.zip (0 :| [1 ..])

digitsToInt :: (Battery, Battery) -> Int
digitsToInt (a, b) = fromIntegral $ 10 * a + b
