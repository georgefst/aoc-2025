module Puzzles.Day6 (puzzle) where

import Pre

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 6
        , parser = const do
            ints <- some ((Just <$> digit) <|> (single ' ' $> Nothing)) `sepEndBy1` newline
            ops <- ((single '*' $> Multiply) <|> (single '+' $> Add)) `sepEndBy` hspace1
            void newline
            pure (ops, ints)
        , parts =
            ( sum
                . uncurry (zipWith applyToList)
                . second (transpose . map (map (digitsToInt @Int . catMaybes) . filter notNull . splitOn [Nothing]))
            )
                /\ ( sum
                        . uncurry (zipWith applyToList)
                        . second
                            ( map catMaybes
                                . splitOn [Nothing]
                                . map (\l -> if all isNothing l then Nothing else Just $ digitsToInt @Int $ catMaybes l)
                                . transpose
                            )
                   )
                /\ nil
        , extraTests = mempty
        }

data Op = Add | Multiply
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, NFData)
apply :: Op -> Int -> Int -> Int
apply = \case
    Add -> (+)
    Multiply -> (*)
unit :: Op -> Int
unit = \case
    Add -> 0
    Multiply -> 1
applyToList :: Op -> [Int] -> Int
applyToList op = foldl' (apply op) (unit op)
