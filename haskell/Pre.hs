{-# LANGUAGE PackageImports #-}

module Pre (
    module BasePrelude,
    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.Loops,
    module Control.Monad.State,
    module Data.Bifunctor,
    module Data.Bool,
    module Data.Char,
    module Data.Foldable,
    module Data.Foldable1,
    module Data.Function,
    module Data.Functor,
    module Data.List,
    module Data.List.Extra,
    module Data.List.NonEmpty,
    module Data.Maybe,
    module Data.Ord,
    module Data.Sequence,
    module Data.Stream.Infinite,
    module Data.Text,
    module Data.Text.Encoding,
    module Data.Traversable,
    module Data.Tuple.Extra,
    module Data.Void,
    module Data.Word,
    module Linear,
    module Safe,
    module Test.Tasty,
    module Test.Tasty.Golden,
    module Test.Tasty.HUnit,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Char.Lexer,
    Puzzle (..),
    digit,
    digitsToInt,
    listIndex,
    allUnorderedPairs,
    adjacentPairs,
    sortPair,
    diffCommand,
)
where

import "base" Prelude as BasePrelude hiding (
    foldl1,
    foldr1,
    head,
    init,
    last,
    maximum,
    minimum,
    tail,
    unzip,
    (!!),
 )

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Foldable hiding (foldl1, foldr1, maximum, maximumBy, minimum, minimumBy)
import Data.Foldable1
import Data.Function
import Data.Functor
import Data.List (sortOn, transpose)
import Data.List.Extra (dropEnd, enumerate, firstJust, notNull, splitOn)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, some1, tail, tails)
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq)
import Data.Stream.Infinite (Stream ((:>)))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable
import Data.Tuple.Extra ((&&&))
import Data.Void
import Data.Word
import Linear (V2 (..))
import Safe
import Test.Tasty
import Test.Tasty.Golden hiding (Always)
import Test.Tasty.HUnit
import Text.Megaparsec hiding (Pos, State, Stream, many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Puzzle = forall input output. (Show output) => Puzzle
    { number :: Word
    , parser :: Bool -> Parsec Void Text input
    , parts :: [input -> output]
    , extraTests :: Bool -> FilePath -> IO input -> [TestTree]
    }

digit :: (Token s ~ Char, Num b, MonadParsec e s f) => f b
digit = fromIntegral . digitToInt <$> digitChar

digitsToInt :: (Integral a) => [a] -> Int
digitsToInt = snd . foldr (\b (p, acc) -> (10 * p, acc + fromIntegral b * p)) (1, 0)

listIndex :: Int -> [a] -> Maybe a
listIndex n =
    if n < 0
        then const Nothing
        else \case
            [] -> Nothing
            x : xs -> if n == 0 then Just x else listIndex (n - 1) xs

allUnorderedPairs :: Bool -> [a] -> [(a, a)]
allUnorderedPairs diagonals = concat . join (zipWith (flip $ map . (,)) . (bool tail toList diagonals) . tails)

adjacentPairs :: [b] -> [(b, b)]
adjacentPairs = \case
    [] -> []
    x : xs -> zip (x : xs) xs

sortPair :: (Ord a) => (a, a) -> (a, a)
sortPair (a, b) = if a <= b then (a, b) else (b, a)

diffCommand :: FilePath -> FilePath -> [String]
diffCommand a b = ["diff", "--color=always", a, b]
