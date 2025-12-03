module Main (main) where

import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Puzzle
import Puzzles.Day1 qualified as Day1
import Puzzles.Day2 qualified as Day2
import Puzzles.Day3 qualified as Day3
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Ingredients.ConsoleReporter
import Text.Megaparsec hiding (Pos)

main :: IO ()
main =
    defaultMain
        . localOption (Always :: UseColor)
        . testGroup "tests"
        $ ["examples", "real"] <&> \t ->
            testGroup t $
                [ Day1.puzzle
                , Day2.puzzle
                , Day3.puzzle
                ]
                    <&> \Puzzle{number, parser, parts} ->
                        let
                            pt = show number
                            parseFile fp =
                                either (fail . ("parse failure: " <>) . errorBundlePretty) pure
                                    . runParser (parser <* eof) fp
                                    =<< T.readFile fp
                         in
                            withResource (parseFile $ "../inputs/" <> t <> "/" <> pt) mempty \input ->
                                testGroup pt $
                                    zip (map show [1 :: Int ..]) parts <&> \(n, pp) ->
                                        goldenVsString n ("../outputs/" <> t <> "/" <> pt <> "/" <> n) $
                                            BL.fromStrict . encodeUtf8 . pp <$> input
