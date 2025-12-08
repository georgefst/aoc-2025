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
    module Data.Void,
    module Data.Word,
    module Linear,
    module Test.Tasty,
    module Test.Tasty.Golden,
    module Test.Tasty.HUnit,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Char.Lexer,
    Puzzle (..),
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
import Data.Functor
import Data.List (sortOn, tails, transpose)
import Data.List.Extra (dropEnd, enumerate)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, some1)
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq)
import Data.Stream.Infinite (Stream ((:>)))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Traversable
import Data.Void
import Data.Word
import Linear (V2 (..))
import Test.Tasty
import Test.Tasty.Golden hiding (Always)
import Test.Tasty.HUnit
import Text.Megaparsec hiding (Pos, State, Stream, many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Puzzle = forall input. Puzzle
    { number :: Word
    , parser :: Parsec Void Text input
    , parts :: [input -> TL.Text]
    , extraTests :: Bool -> FilePath -> IO input -> [TestTree]
    }
