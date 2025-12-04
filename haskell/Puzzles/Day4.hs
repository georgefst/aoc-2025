module Puzzles.Day4 (puzzle) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.List.Extra
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Linear
import Puzzle
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 4
        , parser = flip sepEndBy newline $ some $ asum $ enumerate <&> \t -> char (inToChar t) $> t
        , parts =
            [ T.show
                . fst
                . findAccessible
                . addCoords
            , T.show
                . sum
                . unfoldr ((\r -> guard (fst r /= 0) $> r) . (removeAccessibleRolls <<<<$>>>>) . findAccessible)
                . addCoords
            ]
        }

addCoords :: (Num a, Enum a) => [[c]] -> [[(V2 a, c)]]
addCoords = zipWith (map . first . V2) [0 ..] . map (zip [0 ..])

findAccessible :: [[(V2 Int, InTile)]] -> (Int, [[(V2 Int, OutTile)]])
findAccessible inGrid =
    let
        gridSeq = Seq.fromList $ map Seq.fromList inGrid
        outGrid =
            inGrid <<&>> \(v, t) -> (v,) case t of
                InEmpty -> OutEmpty
                InRoll ->
                    if length (filter ((== Just InRoll) . fmap snd) neighbours) < 4
                        then OutAccessible
                        else OutRoll
                  where
                    neighbours = do
                        x <- [-1 .. 1]
                        y <- [-1 .. 1]
                        guard $ not (x == 0 && y == 0)
                        let V2 x' y' = v + V2 x y
                        pure $ Seq.lookup x' gridSeq >>= Seq.lookup y'
        accessibleRolls = length $ concatMap (filter (== OutAccessible) . map snd) outGrid
     in
        (accessibleRolls, outGrid)

data InTile
    = InEmpty
    | InRoll
    deriving (Eq, Ord, Show, Enum, Bounded)
inToChar :: InTile -> Char
inToChar = \case
    InEmpty -> '.'
    InRoll -> '@'
drawGridIn :: [[InTile]] -> String
drawGridIn = unlines . map (map inToChar)

data OutTile
    = OutEmpty
    | OutRoll
    | OutAccessible
    deriving (Eq, Ord, Show, Enum, Bounded)
outToChar :: OutTile -> Char
outToChar = \case
    OutEmpty -> '.'
    OutRoll -> '@'
    OutAccessible -> 'x'
drawGridOut :: [[OutTile]] -> String
drawGridOut = unlines . map (map outToChar)

removeAccessibleRolls :: OutTile -> InTile
removeAccessibleRolls = \case
    OutEmpty -> InEmpty
    OutRoll -> InRoll
    OutAccessible -> InEmpty

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap
(<<&>>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)
(<<<$>>>) :: (Functor f, Functor f1, Functor f2) => (a -> b) -> f (f1 (f2 a)) -> f (f1 (f2 b))
(<<<$>>>) = fmap . (<<$>>)
(<<<<$>>>>) :: (Functor f1, Functor f2, Functor f3, Functor f4) => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
(<<<<$>>>>) = fmap . (<<<$>>>)
