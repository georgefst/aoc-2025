module Main (main) where

import Pre

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

main :: IO ()
main = do
    -- terminalWidth <- Terminal.Size.width <<$>> Terminal.Size.size -- TODO this doesn't work in GHCID or GHCIWatch...
    terminalWidth <- pure $ Just 60
    putStrLn $ drawTree $ show <$> getTestTree tests
    TL.putStrLn . displayTestResultsConsole terminalWidth =<< runTests () tests

tests :: TestTree IO ()
tests =
    test "tests" pure $
        enumerate <&> \isRealData@(bool "examples" "real" -> t) ->
            test (mkTestName $ T.pack t) pure $
                [ Day1.puzzle
                , Day2.puzzle
                , Day3.puzzle
                , Day4.puzzle
                , Day5.puzzle
                , Day6.puzzle
                , Day7.puzzle
                , Day8.puzzle
                , Day9.puzzle
                , Day10.puzzle
                ]
                    <&> \Puzzle{number = show -> pt, parser, parts, extraTests} ->
                        testLazy
                            (mkTestName $ T.pack pt)
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
                                        (mkTestName $ T.pack nt)
                                        (\(_, rs, _) -> golden ("../outputs/" <> t <> "/" <> pt <> "/" <> nt) $ rs n <> "\n")
                                        []
                              )
                                <> let ts = extraTests isRealData ("../outputs/" <> t <> "/" <> pt <> "/extra/")
                                    in if null ts
                                        then []
                                        else [testLazy "extra" (\(input, _, os) -> pure (input, os)) ts]
