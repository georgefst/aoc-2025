module Main (main) where

import Pre

import Data.Functor.Contravariant
import Data.List ((!!))
import Data.Text.IO qualified as T
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
import Text.Pretty.Simple (pPrintForceColor)

main :: IO ()
main =
    (pPrintForceColor =<<) $ runTests () $ TestTree "tests" pure $ flip map enumerate \isRealData@(bool "examples" "real" -> t) ->
        TestTree (mkTestName t) pure $ flip
            map
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
            \Puzzle{number = show -> pt, parser, parts, extraTests} ->
                    TestTree
                        (mkTestName pt)
                        ( \() -> do
                            let fp = "../inputs/" <> t <> "/" <> pt
                            input <-
                                either (fail . ("parse failure: " <>) . errorBundlePretty) pure
                                    . runParser (parser isRealData <* eof) fp
                                    =<< T.readFile fp
                            let (rs, os) =
                                    (foldHListF0 ((:) . fst) [] &&& foldHListF (HCons . snd) HNil) $
                                        mapHListF (\(Fanout (f, Op o)) -> (o &&& id) $ f input) parts
                             in pure (input, rs, os)
                        )
                        $ ( flip map ([0 :: Int .. hlistfLength parts - 1]) $
                                \n@(show . succ -> nt) ->
                                    TestTree
                                        (mkTestName nt)
                                        ( \(_, rs, _) -> do
                                            golden ("../outputs/" <> t <> "/" <> pt <> "/" <> nt) $ (rs !! n) <> "\n"
                                        )
                                        []
                          )
                            <> [ TestTree
                                    "extra"
                                    (\(input, _, os) -> pure (input, os))
                                    $ extraTests isRealData ("../outputs/" <> t <> "/" <> pt <> "/extra/")
                               ]
