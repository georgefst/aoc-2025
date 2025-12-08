module Main (main) where

import Pre

import Data.Text.IO qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Puzzles.Day1 qualified as Day1
import Puzzles.Day2 qualified as Day2
import Puzzles.Day3 qualified as Day3
import Puzzles.Day4 qualified as Day4
import Puzzles.Day5 qualified as Day5
import Test.Tasty.Ingredients.ConsoleReporter

main :: IO ()
main =
    defaultMain
        . localOption (Always :: UseColor)
        . testGroup "tests"
        $ enumerate <&> \isRealData@(bool "examples" "real" -> t) ->
            testGroup t $
                [ Day1.puzzle
                , Day2.puzzle
                , Day3.puzzle
                , Day4.puzzle
                , Day5.puzzle
                ]
                    <&> \Puzzle{number, parser, parts, extraTests} ->
                        let
                            pt = show number
                            parseFile fp =
                                either (fail . ("parse failure: " <>) . errorBundlePretty) pure
                                    . runParser (parser <* eof) fp
                                    =<< T.readFile fp
                         in
                            withResource (parseFile $ "../inputs/" <> t <> "/" <> pt) mempty \input ->
                                testGroup pt $
                                    ( zip (map show [1 :: Int ..]) parts <&> \(n, pp) ->
                                        goldenVsString n ("../outputs/" <> t <> "/" <> pt <> "/" <> n) $
                                            TL.encodeUtf8 . pp <$> input
                                    )
                                        <> [testGroup "extra" $ extraTests isRealData ("../outputs/" <> t <> "/" <> pt <> "/extra/") input]
