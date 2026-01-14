{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Pre

import Data.Bits
import Data.Time

import Data.Finite
import Data.Functor.Contravariant
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
import Puzzles.Day1 qualified as Day1
import Puzzles.Day10 qualified as Day10
import Puzzles.Day2 qualified as Day2
import Puzzles.Day3 qualified as Day3
import Puzzles.Day4 qualified as Day4
import Puzzles.Day5 qualified as Day5
import Puzzles.Day6 qualified as Day6
import Puzzles.Day7 qualified as Day7
import Puzzles.Day8 qualified as Day8
import Puzzles.Day9 qualified as Day9
import System.Console.Terminal.Size qualified as Terminal.Size

import Debug.Pretty.Simple (pTraceShowId, pTraceShowIdForceColor, pTraceShowWith, pTraceWith)
import GHC.Exts
import Numeric (showHex)
import System.Console.ANSI
import System.Console.Terminal.Size (Window (Window), hSize)
import System.Console.Terminal.Size qualified
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.IO (IOMode (ReadMode), stdout, withFile)
import Text.Pretty.Simple
import Text.Read
import Unsafe.Coerce (unsafeCoerce)

-- TODO upstream
-- it is a bit of a shame that we can't easily allow for just one of the vars to exist, without changing the API
sizeFromEnv :: IO (Maybe (Window Int))
sizeFromEnv = do
    rows <- lookupEnv "LINES"
    cols <- lookupEnv "COLUMNS"
    pure do
        r <- rows >>= readMaybe
        c <- cols >>= readMaybe
        pure $ Window r c

main :: IO ()
main = do
    -- terminalWidth <- Terminal.Size.width <<$>> Terminal.Size.size -- TODO this doesn't work in GHCID or GHCIWatch...
    -- terminalWidth <- Terminal.Size.width <<$>> sizeFromEnv
    terminalWidth <- pure $ Just 62
    -- TL.putStrLn . displayTestResultsConsole (intToFilledBar . testDurationBarFunction) terminalWidth
    TL.putStrLn . displayTestResultsConsole terminalWidth
        =<< runTests
            TestRunnerOpts
                { regenerateGoldenFiles = False
                -- { regenerateGoldenFiles = True
                }
            ()
            tests

tests :: TestTree IO ()
tests =
    test "tests" pure $
        -- [False] <&> \isRealData@(bool "examples" "real" -> t) ->
        enumerate <&> \isRealData@(bool "examples" "real" -> t) ->
            test (T.pack t) pure $
                [ Day1.puzzle
                -- , Day2.puzzlex
                , Day3.puzzle
                -- , Day4.puzzle
                , Day5.puzzle
                , Day6.puzzle
                , Day7.puzzle
                -- , Day8.puzzle
                -- , Day9.puzzle
                -- , Day10.puzzle
                ]
                    <&> \Puzzle{number = show -> pt, parser, parts, extraTests} ->
                        testLazy
                            (T.pack pt)
                            ( \() -> do
                                let fp = "../inputs/" <> t <> "/" <> pt
                                input <-
                                    either (assertFailure . T.pack . ("parse failure: " <>) . errorBundlePretty) pure
                                        . runParser (parser isRealData <* eof) fp
                                        =<< liftIO (T.readFile fp)
                                let (rs, os) =
                                        (lookupHList fst &&& foldHListF (HCons . snd) HNil) $
                                            mapHListF (\((Fanout (f, Op o))) -> (o &&& id) $ f input) parts
                                 in pure (input, rs, os)
                            )
                            $ ( finites <&> \(n@(show . succ @Int . fromIntegral -> nt)) ->
                                    test
                                        (T.pack nt)
                                        (\(_, rs, _) -> golden ("../outputs/" <> t <> "/" <> pt <> "/" <> nt) $ rs n <> "\n")
                                        []
                              )
                                <> let ts = extraTests isRealData ("../outputs/" <> t <> "/" <> pt <> "/extra/")
                                    in if null ts
                                        then []
                                        else [testLazy "extra" (\(input, _, os) -> pure (input, os)) ts]
