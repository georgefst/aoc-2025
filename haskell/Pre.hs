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
    drawTree,
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
    Test,
    test,
    testLazy,
    TestName,
    getTestTree,
    displayTestResultsConsole,
    TestRunnerOpts (..),
    runTests,
    assertEqual,
    assert,
    assertFailure,
    golden,
    exampleTestResult,
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
import Control.Monad.Catch (MonadCatch, MonadThrow, try)
import Control.Monad.Except
import Control.Monad.Loops hiding (firstM)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
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
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Traversable
import Data.Tree hiding (drawTree)
import Data.Tuple.Extra (firstM, (&&&))
import Data.Void
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat, type (+))
import Linear (V2 (..))
import Safe
import System.Console.ANSI
import System.Directory
import System.FilePath
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

{- | This is `Data.Tree.drawTree` with the ASCII characters replaced with Unicode box drawing characters,
and using `Text` instead of `String`.
-}
drawTree :: Tree Text -> Text
drawTree = T.unlines . draw
  where
    draw (Node x ts0) = T.lines x <> drawSubTrees ts0
      where
        drawSubTrees [] = []
        drawSubTrees [t] =
            "│" : shift_ "└─ " "   " (draw t)
        drawSubTrees (t : ts) =
            "│" : shift_ "├─ " "│  " (draw t) <> drawSubTrees ts
        shift_ first_ other = zipWith (<>) (first_ : repeat other)

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
    TestCase :: (NFData output) => (input -> Test m output) -> TestCase m input output
    TestCaseLazy :: (input -> Test m output) -> TestCase m input output

newtype Test m a
    = Test
        ( ExceptT
            TestFailure
            ( WriterT
                [TestLogItem]
                (ReaderT TestRunnerOpts m)
            )
            a
        )
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadThrow
        , MonadCatch
        , MonadError TestFailure
        , MonadWriter [TestLogItem]
        , MonadReader TestRunnerOpts
        )

-- | See `testLazy` for avoiding the `NFData` constraint.
test :: (NFData output) => Text -> (input -> Test m output) -> [TestTree m output] -> TestTree m input
test n f = TestTree (TestName n) $ TestCase f

{- | This is `test` without the `NFData` constraint.
It doesn't force the output before completion, which means that reported timings may be less accurate.
-}
testLazy :: Text -> (input -> Test m output) -> [TestTree m output] -> TestTree m input
testLazy n f = TestTree (TestName n) $ TestCaseLazy f

data TestResult = TestResult
    { name :: TestName
    , logs :: [TestLogItem]
    , result :: Either TestFailure (NominalDiffTime, [TestResult])
    }

data TestLogItem
    = LogRegeneratedGolden

data TestFailure
    = ExceptionFailure SomeException
    | AssertionFailure Text
    | GoldenMissing
    | GoldenFailure {expected :: Text, actual :: Text}

newtype TestName = TestName Text

getTestTree :: TestTree m r -> Tree Text
getTestTree (TestTree (TestName name) _ ts) = Node name $ map getTestTree ts

displayTestResultsConsole :: (NominalDiffTime -> Char) -> Maybe Int -> TestResult -> TL.Text
displayTestResultsConsole timeBarFunction terminalWidth testResult =
    displayResult 0 testResult <> TL.pack (setSGRCode [Reset])
  where
    displayResult indent =
        (TL.replicate (fromIntegral indent) "  " <>) . \case
            TestResult{name = TestName name, logs, result} ->
                case result of
                    Right (dt, children) ->
                        TL.fromStrict (header Green '✓' name indent (Just dt) <> displayLogs)
                            <> TL.concat (map (displayResult (indent + 1)) children)
                    Left e ->
                        TL.fromStrict $
                            header Red '✗' name indent Nothing
                                <> displayLogs
                                <> setColour Vivid Red
                                <> indentAllLines indent case e of
                                    ExceptionFailure ex -> T.show ex
                                    AssertionFailure t -> T.stripEnd t
                                    GoldenMissing -> "Golden file missing"
                                    GoldenFailure{expected, actual} ->
                                        "Expected:\n" <> T.stripEnd expected <> "\nActual:\n" <> T.stripEnd actual
              where
                displayLogs =
                    setColour Dull Magenta
                        <> indentAllLines
                            indent
                            ( flip foldMap logs \case
                                LogRegeneratedGolden -> "Created golden file"
                            )
                        <> setColour Dull Magenta
    header colour icon name indent time =
        setColour Vivid colour
            <> T.singleton icon
            <> " "
            <> setColour Dull White
            <> name
            <> maybe
                mempty
                ( \t@(showTime -> tt) ->
                    T.replicate
                        ( fromIntegral $
                            maybe
                                3
                                (\n -> n - (2 * indent + T.length name + T.length tt + 4))
                                terminalWidth
                        )
                        " "
                        <> setColour Dull Blue
                        <> tt
                        <> " "
                        <> T.singleton (timeBarFunction t)
                )
                time
            <> "\n"
    paddedAllLines p = T.unlines . map (p <>) . T.lines
    indentAllLines indent = paddedAllLines $ T.replicate (indent * 2) " "
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

data TestRunnerOpts = TestRunnerOpts
    { regenerateGoldenFiles :: Bool
    }

runTests :: (MonadIO m, MonadCatch m) => TestRunnerOpts -> a -> TestTree m a -> m TestResult
runTests opts r0 (TestTree name tc ts) =
    let Test t = Control.Monad.Catch.try $ runTest tc
     in runReaderT (runWriterT (runExceptT t)) opts
            >>= fmap (\(result, logs) -> TestResult{name, logs, result}) . firstM \case
                Left e ->
                    pure $ Left e
                Right (Left e) ->
                    pure $ Left $ ExceptionFailure e
                Right (Right (r, dt)) ->
                    Right . (dt,) <$> for ts (runTests opts r)
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

assertEqual :: (Eq p, Monad m) => p -> p -> Test m ()
assertEqual expected actual = assert "not equal" (expected == actual)
assert :: (Monad m) => Text -> Bool -> Test m ()
assert s b = if b then pure () else assertFailure s
assertFailure :: (Monad m) => Text -> Test m a
assertFailure = throwError . AssertionFailure
golden :: (MonadIO m, MonadFail m) => FilePath -> Text -> Test m ()
golden file actual = do
    TestRunnerOpts{..} <- ask
    exists <- liftIO $ doesFileExist file
    if exists
        then do
            expected <- liftIO $ T.readFile file
            if expected == actual then pure () else throwError $ GoldenFailure{expected, actual}
        else do
            if regenerateGoldenFiles
                then
                    let parents = dropWhile null $ scanl (</>) "" $ splitDirectories $ takeDirectory file
                     in tell [LogRegeneratedGolden] >> liftIO do
                            for_ parents \dir -> do
                                parentExists <- liftIO $ doesDirectoryExist dir
                                when (not parentExists) $ createDirectory dir
                            T.writeFile file actual
                else
                    throwError GoldenMissing

exampleTestResult :: TestResult
exampleTestResult =
    TestResult
        { name = TestName "tests"
        , logs = []
        , result =
            Right
                ( 0.000008967
                ,
                    [ TestResult
                        { name = TestName "examples"
                        , logs = []
                        , result =
                            Right
                                ( 0.000002405
                                ,
                                    [ TestResult
                                        { name = TestName "1"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000114605
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000058089
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000023895
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "2"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000027692
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000040646
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000048722
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "3"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000036248
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000031349
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000029976
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "4"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000031709
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000103133
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000343045
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "extra"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000001883
                                                                ,
                                                                    [ TestResult
                                                                        { name = TestName "round trip"
                                                                        , logs = []
                                                                        , result =
                                                                            Right
                                                                                ( 0.000040245
                                                                                , []
                                                                                )
                                                                        }
                                                                    , TestResult
                                                                        { name = TestName "frames"
                                                                        , logs = []
                                                                        , result =
                                                                            Right
                                                                                ( 0.000034855
                                                                                ,
                                                                                    [ TestResult
                                                                                        { name = TestName "0"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.0000373
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "1"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000025197
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "2"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000024947
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "3"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000022812
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "4"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000021851
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "5"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000021821
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "6"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000023013
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "7"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00002124
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "8"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00002128
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "9"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000023034
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "end"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00000538
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    ]
                                                                                )
                                                                        }
                                                                    ]
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "5"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000024426
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000021851
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000030337
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "6"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000033443
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000030217
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000024476
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "7"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000032972
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000038723
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000029566
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "8"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000030387
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000100248
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000101951
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "9"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000025198
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000030046
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000600027
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    ]
                                )
                        }
                    , TestResult
                        { name = TestName "real"
                        , logs = []
                        , result =
                            Right
                                ( 0.000001943
                                ,
                                    [ TestResult
                                        { name = TestName "1"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.001730131
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.001631867
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000546617
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "2"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000073909
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.251483373
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.422864155
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "3"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.004100956
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.001739268
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.001751551
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "4"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.001411282
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.010032562
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.349189653
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "extra"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000001523
                                                                ,
                                                                    [ TestResult
                                                                        { name = TestName "round trip"
                                                                        , logs = []
                                                                        , result =
                                                                            Right
                                                                                ( 0.001444965
                                                                                , []
                                                                                )
                                                                        }
                                                                    , TestResult
                                                                        { name = TestName "frames"
                                                                        , logs = []
                                                                        , result =
                                                                            Right
                                                                                ( 0.030300588
                                                                                ,
                                                                                    [ TestResult
                                                                                        { name = TestName "0"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.001072555
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "1"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000784895
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "2"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000925199
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "3"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000775888
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "4"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000911994
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "5"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000780266
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "6"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000891385
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "7"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000775718
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "8"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000858043
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "9"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00071276
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "10"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000846681
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "11"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000708832
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "12"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000833266
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "13"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00070235
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "14"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00086194
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "15"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000694795
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "16"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000840269
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "17"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000674768
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "18"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000817336
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "19"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000684727
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "20"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000801185
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "21"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000669007
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "22"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000816875
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "23"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000689546
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "24"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000825851
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "25"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000691309
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "26"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000843916
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "27"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000667154
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "28"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000896264
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "29"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000695257
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "30"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000846381
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "31"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000696779
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "32"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000822746
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "33"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000682302
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "34"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000813198
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "35"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00088887
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "36"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000719391
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "37"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000936059
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "38"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000708822
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "39"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00088882
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "40"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000709182
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "41"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000832534
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "42"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000702901
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "43"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000808809
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "44"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000690618
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "45"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.00079803
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "46"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000703742
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "47"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000825772
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "48"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000719441
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "49"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000810694
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "50"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000692491
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "51"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000803229
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "52"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000680669
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "53"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000785686
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "54"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000641956
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "55"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000732787
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "56"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000543541
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "57"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000576974
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "58"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000428114
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    , TestResult
                                                                                        { name = TestName "end"
                                                                                        , logs = []
                                                                                        , result =
                                                                                            Right
                                                                                                ( 0.000199855
                                                                                                , []
                                                                                                )
                                                                                        }
                                                                                    ]
                                                                                )
                                                                        }
                                                                    ]
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "5"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.0004574
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000550965
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000062508
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "6"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.002074248
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000967398
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000650112
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "7"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.00114374
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000747876
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.000265468
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "8"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.000442361
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.100017142
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.083740998
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    , TestResult
                                        { name = TestName "9"
                                        , logs = []
                                        , result =
                                            Right
                                                ( 0.0006119
                                                ,
                                                    [ TestResult
                                                        { name = TestName "1"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.005058044
                                                                , []
                                                                )
                                                        }
                                                    , TestResult
                                                        { name = TestName "2"
                                                        , logs = []
                                                        , result =
                                                            Right
                                                                ( 0.127813254
                                                                , []
                                                                )
                                                        }
                                                    ]
                                                )
                                        }
                                    ]
                                )
                        }
                    ]
                )
        }
