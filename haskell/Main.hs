module Main (main) where

import Pre

import Data.Functor.Contravariant
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

main :: IO ()
main =
    sydTest $ doNotRandomiseExecutionOrder $ for_ enumerate \isRealData@(bool "examples" "real" -> t) ->
        describe t $ for_
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
            \Puzzle{number, parser, parts, extraTests} ->
                let
                    pt = show number
                    parseFile fp =
                        either (fail . ("parse failure: " <>) . errorBundlePretty) pure
                            . runParser (parser isRealData <* eof) fp
                            =<< T.readFile fp
                 in
                    describe pt do
                        input <- liftIO $ parseFile $ "../inputs/" <> t <> "/" <> pt
                        let (rs, os) =
                                (foldHListF0 ((:) . fst) [] &&& foldHListF (HCons . snd) HNil) $
                                    mapHListF (\(Fanout (f, Op o)) -> (o &&& id) $ f input) parts
                        for_ (zip [1 :: Int ..] rs) $ uncurry $ \(show -> n) ->
                            it n . pureGoldenTextFile ("../outputs/" <> t <> "/" <> pt <> "/" <> n) . (<> "\n")
                        describe "extra" $ extraTests isRealData ("../outputs/" <> t <> "/" <> pt <> "/extra/") input os
