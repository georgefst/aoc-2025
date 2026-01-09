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
                . second (transpose . map (map (digitsToInt @Int) . filter notNull . groupJusts))
            )
                /\ ( sum
                        . uncurry (zipWith applyToList)
                        . second
                            ( groupJusts
                                . map (\l -> if all isNothing l then Nothing else Just $ digitsToInt @Int $ catMaybes l)
                                . transpose
                            )
                   )
                /\ nil
        , extraTests = mempty
        }

data Op = Add | Multiply
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, NFData)
apply :: (Num a) => Op -> a -> a -> a
apply = \case
    Add -> (+)
    Multiply -> (*)
unit :: (Num a) => Op -> a
unit = \case
    Add -> 0
    Multiply -> 1
applyToList :: (Num a) => Op -> [a] -> a
applyToList op = foldl' (apply op) (unit op)

groupJusts :: (Eq a) => [Maybe a] -> [[a]]
groupJusts = map catMaybes . splitOn [Nothing]
