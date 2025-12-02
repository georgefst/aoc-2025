module Main (main) where

import Control.Monad.State
import Data.Bifunctor
import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Void
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

main :: IO ()
main =
    defaultMain $
        testGroup
            "tests"
            [ puzzleTest puzzle1
            ]

puzzleTest :: Puzzle a -> TestTree
puzzleTest p =
    testGroup pt $
        ["examples", "real"] <&> \t ->
            withResource (parseFile $ "inputs/" <> t <> "/" <> pt) mempty \input ->
                testGroup t $
                    [("1", p.part1), ("2", p.part2)] <&> \(n, pp) ->
                        goldenVsString n ("outputs/" <> t <> "/" <> pt <> "/" <> n) $
                            BL.fromStrict . encodeUtf8 . pp.solve <$> input
  where
    pt = show p.number
    parseFile fp = maybe (fail "parse failure") pure . parseMaybe (p.parser <* eof) =<< T.readFile fp

data Puzzle input = Puzzle
    { number :: Word
    , parser :: Parsec Void Text input
    , part1 :: Part input
    , part2 :: Part input
    }
data Part input = Part
    { solve :: input -> Text
    , expected :: Text
    }

puzzle1 :: Puzzle [(Direction, Inc)]
puzzle1 =
    Puzzle
        { number = 1
        , parser = flip sepEndBy newline $ (,) <$> ((char 'L' $> L) <|> (char 'R' $> R)) <*> (Inc <$> Lex.decimal)
        , part1 =
            Part
                { solve =
                    T.show
                        . sum
                        . flip evalState 50
                        . traverse \(d, i) -> state \p ->
                            let (_, p') = step i d p
                             in (Count if p' == 0 then 1 else 0, p')
                , expected = "3"
                }
        , part2 =
            Part
                { solve =
                    T.show
                        . sum
                        . flip evalState 50
                        . traverse \(d, i) -> state \p ->
                            let (c, p') = step i d p
                                c' = case d of
                                    R -> abs c
                                    L ->
                                        if
                                            | p == 0 -> abs c - 1
                                            | p' == 0 -> abs c + 1
                                            | otherwise -> abs c
                             in (c', p')
                , expected = "6"
                }
        }

data Direction = L | R
    deriving (Eq, Ord, Show)

newtype Pos = Pos Int
    deriving newtype (Eq, Ord, Show, Num)

newtype Inc = Inc Int
    deriving newtype (Eq, Ord, Show, Num)

newtype Count = Count Int
    deriving newtype (Eq, Ord, Show, Num)

step :: Inc -> Direction -> Pos -> (Count, Pos)
step (Inc i) d (Pos p) = bimap Count Pos case d of
    L -> (p - i) `divMod` 100
    R -> (p + i) `divMod` 100
