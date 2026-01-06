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
    module Data.Tree,
    module Data.Tuple.Extra,
    module Data.Void,
    module Data.Word,
    module GHC.Generics,
    module Linear,
    module Safe,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Char.Lexer,
    module Text.Pretty.Simple,
    Puzzle (..),
    mwhen,
    (<<$>>),
    (<<&>>),
    takeUntil,
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
    TestTree,
    test,
    testLazy,
    TestName,
    mkTestName,
    getTestTree,
    displayTestResultsConsole,
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
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable hiding (foldl1, foldr1, maximum, maximumBy, minimum, minimumBy)
import Data.Foldable1
import Data.Function
import Data.Functor
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Functor.Contravariant
import Data.Kind (Constraint, Type)
import Data.List (List, genericLength, sortOn, transpose)
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
import Data.Text.Lazy qualified as TL
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
import System.Console.ANSI
import Text.Megaparsec hiding (Pos, State, Stream, many, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Pretty.Simple (pPrint, pPrintForceColor, pShow)

data Puzzle = forall input outputs. (KnownNat (Length outputs), NFData input) => Puzzle
    { number :: Word
    , parser :: Bool -> Parsec Void Text input
    , parts :: PuzzleParts input outputs
    , extraTests :: Bool -> FilePath -> [TestTree IO (input, HList outputs)]
    }

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap
(<<&>>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)

mwhen :: (Monoid p) => Bool -> p -> p
mwhen b x = if b then x else mempty

takeUntil :: (Foldable t) => (a -> Bool) -> t a -> [a]
takeUntil p = foldr (\x xs -> x : if p x then [] else xs) []

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

type PuzzleParts input = HListF ((Fanout ((->) input) (Op Text)))
infixr 9 /\\
(/\\) :: (input -> output, output -> Text) -> PuzzleParts input outputs -> PuzzleParts input (output : outputs)
(/\\) (f, o) = HConsF $ Fanout (f, Op o)
infixr 9 /\
(/\) :: (Show output) => (input -> output) -> PuzzleParts input outputs -> PuzzleParts input (output : outputs)
(/\) f = HConsF $ Fanout (f, Op T.show)
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
    TestTree :: TestName -> TestCase m input output -> [TestTree m output] -> TestTree m input

data TestCase m input output where
    TestCase :: (NFData output) => (input -> m output) -> TestCase m input output
    TestCaseLazy :: (input -> m output) -> TestCase m input output

-- | See `testLazy` for avoiding the `NFData` constraint.
test :: (NFData output) => TestName -> (input -> m output) -> [TestTree m output] -> TestTree m input
test n f = TestTree n $ TestCase f

{- | This is `test` without the `NFData` constraint.
It doesn't force the output before completion, which means that reported timings may be less accurate.
-}
testLazy :: TestName -> (input -> m output) -> [TestTree m output] -> TestTree m input
testLazy n f = TestTree n $ TestCaseLazy f

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

displayTestResultsConsole :: Maybe Int -> TestResult -> TL.Text
displayTestResultsConsole terminalWidth testResult =
    displayResult 0 testResult <> TL.pack (setSGRCode [Reset])
  where
    displayResult indent =
        (TL.replicate (fromIntegral indent) "  " <>) . \case
            Pass (TestName name) dt children ->
                TL.fromStrict (header Green '✓' name indent (Just dt))
                    <> TL.concat (map (displayResult (indent + 1)) children)
            Fail (TestName name) (SomeExceptionLegalShow e) ->
                TL.fromStrict
                    ( header Red '✗' name indent Nothing
                        <> setColour Vivid Red
                    )
                    <> TL.show e
                    <> "\n"
    header colour icon name indent time =
        setColour Vivid colour
            <> T.singleton icon
            <> " "
            <> setColour Dull White
            <> T.pack name
            <> maybe
                mempty
                ( \_t@(showTime -> tt) ->
                    T.replicate
                        ( fromIntegral $
                            maybe
                                3
                                (\n -> n - (2 * indent + length name + T.length tt + 2))
                                terminalWidth
                        )
                        " "
                        <> setColour Dull Black
                        <> tt
                )
                time
            <> "\n"
    showTime (nominalDiffTimeToSeconds -> MkFixed duration) =
        -- SI prefixes, and always exactly 2 decimal places, or 3 if there's no prefix
        T.show res
            <> T.singleton '.'
            <> T.take (if isNothing unit then 3 else 2) (T.show frac <> "000")
            <> foldMap T.singleton unit
            <> T.singleton 's'
      where
        (frac, res, unit) = case duration of
            0 -> (0, 0, Nothing)
            d -> go (0 :: Int) 0 d
        go = \case
            4 -> (,,Nothing)
            iterations -> \carried n ->
                case n `divMod` 1000 of
                    (0, r) ->
                        ( carried
                        , r
                        , Just case iterations of
                            3 -> 'm'
                            2 -> 'μ'
                            1 -> 'n'
                            _ -> 'p'
                        )
                    (d, r) -> go (succ iterations) r d
    sgr = T.pack . setSGRCode
    setColour d c = sgr [SetColor Foreground d c]

runTests :: (MonadIO m, MonadCatch m) => a -> TestTree m a -> m TestResult
runTests r0 (TestTree name tc ts) =
    Control.Monad.Catch.try (runTest tc) >>= \case
        Left e ->
            pure $ Fail name $ SomeExceptionLegalShow e
        Right (r, dt) ->
            Pass name dt <$> for ts (runTests r)
  where
    runTest = \case
        TestCase f -> timed (liftIO . evaluate . DeepSeq.force) $ f r0
        TestCaseLazy f -> timed pure $ f r0
    timed f x = do
        t0 <- liftIO getCurrentTime
        r <- x
        rf <- f r
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
