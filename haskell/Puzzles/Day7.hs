module Puzzles.Day7 (puzzle) where

import Pre

import Data.IntSet qualified as IS
import Data.Text.Lazy qualified as TL

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 7
        , parser = do
            line1 <- some $ (single '.' $> False) <|> (single 'S' $> True)
            start <- maybe (fail "start not found") (pure . fst) $ find snd $ zip [0 ..] line1
            void newline
            rows <- (some $ (single '.' $> False) <|> (single '^' $> True)) `sepEndBy1` newline
            let splitters = map (IS.fromList . map fst . filter snd . zip [0 ..]) rows
            pure (start, splitters)
        , parts =
            [ uncurry \start ->
                TL.show
                    . flip execState (0 :: Int)
                    . foldlM
                        ( \beams splitters ->
                            IS.fromList . concat <$> for (IS.toList beams) \x -> do
                                let hit = x `IS.member` splitters
                                when hit $ modify succ
                                pure if hit then [x - 1, x + 1] else [x]
                        )
                        (IS.singleton start)
            ]
        , extraTests = mempty
        }
