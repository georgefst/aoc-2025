{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pre (
    module BasePrelude,
    module Control.Applicative,
    module Control.DeepSeq,
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
    module Data.Functor.Compose,
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
    module GHC.Generics,
    module Linear,
    module Safe,
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
    HList (..),
    HListC (..),
    HListF (..),
    foldHListF,
    foldHListF0,
    mapHListF,
    lookupHList,
    (/\),
    (/\\),
    nil,
    Constrained (..),
    withConstrained,
    Fanout (..),
    Length,
    TestTree (..),
    TestName,
    mkTestName,
    getTestTree,
    runTests,
    assertEqual,
    assert,
    golden,
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
import Control.DeepSeq (NFData, deepseq)
import Control.DeepSeq qualified as DeepSeq
import Control.Exception (SomeException, evaluate)
import Control.Monad
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Loops
import Control.Monad.State
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Finite
import Data.Foldable hiding (foldl1, foldr1, maximum, maximumBy, minimum, minimumBy)
import Data.Foldable1
import Data.Function
import Data.Functor
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Functor.Contravariant
import Data.Kind (Constraint, Type)
import Data.List (List, sortOn, transpose)
import Data.List.Extra (dropEnd, enumerate, firstJust, notNull, splitOn)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, some1, tail, tails)
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq)
import Data.Stream.Infinite (Stream ((:>)))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Time
import Data.Traversable
import Data.Tree
import Data.Tuple.Extra ((&&&))
import Data.Void
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, type (+))
import Linear (V2 (..))
import Safe
import Text.Megaparsec hiding (Pos, State, Stream, many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Puzzle = forall input outputs. (KnownNat (Length outputs), NFData input) => Puzzle
    { number :: Word
    , parser :: Bool -> Parsec Void Text input
    , parts :: PuzzleParts input outputs
    , extraTests :: Bool -> FilePath -> [TestTree IO (input, HListC NFData outputs)]
    }

digit :: (Token s ~ Char, Num b, MonadParsec e s f) => f b
digit = fromIntegral . digitToInt <$> digitChar

digitsToInt :: (Integral a) => [a] -> Int
digitsToInt = foldl' (\acc d -> acc * 10 + fromIntegral d) 0

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

type PuzzleParts input = HListF (Compose (Fanout ((->) input) (Op Text)) (Constrained NFData))
infixr 9 /\\
(/\\) :: (NFData output) => (input -> output, output -> Text) -> PuzzleParts input outputs -> PuzzleParts input (output : outputs)
(/\\) (f, o) = HConsF $ Compose $ Fanout (Constrained . f, Op $ withConstrained o)
infixr 9 /\
(/\) :: (Show output, NFData output) => (input -> output) -> PuzzleParts input outputs -> PuzzleParts input (output : outputs)
(/\) f = HConsF $ Compose $ Fanout (Constrained . f, Op $ withConstrained T.show)
nil :: PuzzleParts input '[]
nil = HNilF

data HList (as :: List Type) :: Type where
    HNil :: HList '[]
    HCons ::
        a ->
        HList as ->
        HList (a ': as)

data HListC (c :: Type -> Constraint) (as :: List Type) :: Type where
    HNilC :: HListC c '[]
    HConsC ::
        (c a) =>
        a ->
        HListC c as ->
        HListC c (a ': as)
instance NFData (HListC NFData outputs) where
    rnf = \case
        HNilC -> ()
        HConsC x xs -> deepseq x $ DeepSeq.rnf xs

data HListF (f :: Type -> Type) (as :: List Type) :: Type where
    HNilF :: HListF f '[]
    HConsF ::
        f a ->
        HListF f as ->
        HListF f (a ': as)
foldHListF :: (forall x xs. f x -> r xs -> r (x ': xs)) -> r '[] -> HListF f as -> r as
foldHListF f e = \case
    HNilF -> e
    HConsF x xs -> f x $ foldHListF f e xs
foldHListF0 :: (forall x. f x -> r -> r) -> r -> HListF f as -> r
foldHListF0 f e = \case
    HNilF -> e
    HConsF x xs -> f x $ foldHListF0 f e xs
mapHListF :: (forall a. f a -> g a) -> HListF f as -> HListF g as
mapHListF t = foldHListF (\x r -> HConsF (t x) $ r) HNilF

lookupHList :: (forall a. f a -> r) -> HListF f as -> Finite (Length as) -> r
lookupHList f = \case
    HNilF -> absurd . separateZero
    HConsF x xs -> maybe (f x) (lookupHList f xs) . unshift

data Constrained c a where
    Constrained :: (c a) => a -> Constrained c a
withConstrained :: ((c a) => a -> b) -> Constrained c a -> b
withConstrained f (Constrained x) = f x

newtype Fanout f g a = Fanout (f a, g a)

type family Length as :: Nat where
    Length '[] = 0
    Length (x ': xs) = Length xs + 1

data TestTree m input where
    TestTree :: (NFData output) => TestName -> (input -> m output) -> [TestTree m output] -> TestTree m input

data TestResult
    = Pass TestName NominalDiffTime [TestResult]
    | Fail TestName SomeExceptionLegalShow
    deriving (Show)

newtype TestName = TestName String
    deriving newtype (IsString, Show)

mkTestName :: String -> TestName
mkTestName = TestName

newtype SomeExceptionLegalShow = SomeExceptionLegalShow SomeException
instance Show SomeExceptionLegalShow where
    show (SomeExceptionLegalShow e) = show $ show e

getTestTree :: TestTree m r -> Tree TestName
getTestTree (TestTree name _ ts) = Node name $ map getTestTree ts

runTests :: (MonadIO m, MonadCatch m) => a -> TestTree m a -> m TestResult
runTests r0 (TestTree name f ts) =
    Control.Monad.Catch.try (timed $ f r0) >>= \case
        Left e ->
            pure $ Fail name $ SomeExceptionLegalShow e
        Right (r, dt) ->
            Pass name dt <$> for ts (runTests r)
  where
    timed x = do
        t0 <- liftIO getCurrentTime
        r <- x
        rf <- liftIO $ evaluate $ DeepSeq.force r
        t1 <- liftIO getCurrentTime
        pure (rf, diffUTCTime t1 t0)

assertEqual :: (Eq p, MonadFail f) => p -> p -> f ()
assertEqual expected actual = assert "not equal" (expected == actual)
assert :: (MonadFail f) => String -> Bool -> f ()
assert s b = if b then pure () else fail s
golden :: FilePath -> Text -> IO ()
golden p x = do
    expected <- T.readFile p
    if expected == x then pure () else fail "golden test failure"
