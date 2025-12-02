module Main (main) where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Data.Void
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Ingredients.ConsoleReporter
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex

main :: IO ()
main =
    defaultMain
        . localOption (Always :: UseColor)
        . testGroup "tests"
        $ ["examples", "real"] <&> \t ->
            testGroup t $
                [ puzzle1
                , puzzle2
                ]
                    <&> \Puzzle{number, parser, parts} ->
                        let
                            pt = show number
                            parseFile fp =
                                either (fail . ("parse failure: " <>) . errorBundlePretty) pure
                                    . runParser (parser <* eof) fp
                                    =<< T.readFile fp
                         in
                            withResource (parseFile $ "inputs/" <> t <> "/" <> pt) mempty \input ->
                                testGroup pt $
                                    zip (map show [1 :: Int ..]) parts <&> \(n, pp) ->
                                        goldenVsString n ("outputs/" <> t <> "/" <> pt <> "/" <> n) $
                                            BL.fromStrict . encodeUtf8 . pp <$> input

data Puzzle = forall input. Puzzle
    { number :: Word
    , parser :: Parsec Void Text input
    , parts :: [input -> Text]
    }

puzzle1 :: Puzzle
puzzle1 =
    Puzzle
        { number = 1
        , parser = flip sepEndBy newline $ (,) <$> ((char 'L' $> L) <|> (char 'R' $> R)) <*> (Inc <$> Lex.decimal)
        , parts =
            [ T.show
                . sum
                . flip evalState 50
                . traverse \(d, i) -> state \p ->
                    let (_, p') = step i d p
                     in (Count if p' == 0 then 1 else 0, p')
            , T.show
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
            ]
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

puzzle2 :: Puzzle
puzzle2 =
    Puzzle
        { number = 2
        , parser = (<* newline) $ flip sepBy (char ',') $ (,) <$> (Lex.decimal <* char '-') <*> Lex.decimal
        , parts =
            [ T.show
                . sum
                . concatMap
                    (mapMaybe (\n -> guard (isRepetition2 n) $> n) . uncurry enumFromTo)
            , T.show
                . sum
                . concatMap
                    (mapMaybe (\n -> guard (isRepetitionN n) $> n) . uncurry enumFromTo)
            ]
        }

newtype ID = ID Int
    deriving newtype (Eq, Ord, Show, Num, Enum)

isRepetition2 :: ID -> Bool
isRepetition2 (T.show -> n) = case T.length n `divMod` 2 of
    (d, 0) -> equalChunks n d
    _ -> False

isRepetitionN :: ID -> Bool
isRepetitionN (T.show -> n) = flip any [1 .. T.length n `div` 2] $ equalChunks n

equalChunks :: Text -> Int -> Bool
equalChunks n i = case T.chunksOf i n of
    [] -> True
    x : xs -> all (== x) xs
