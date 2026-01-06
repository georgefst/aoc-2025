module Puzzles.Day10 (puzzle) where

import Pre

import Data.IntMap qualified as IM

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 10
        , parser = const $ flip sepEndBy newline do
            void $ single '['
            lights <- some $ (single '.' $> Off) <|> (single '#' $> On)
            void $ single ']'
            void space1
            switches <- flip sepEndBy space1 do
                void $ single '('
                r <- Switch <$> decimal `sepBy` single ','
                void $ single ')'
                pure r
            void $ single '{'
            void $ decimal @_ @_ @_ @Int `sepBy` single ','
            void $ single '}'
            pure (Lights $ IM.fromList $ zip [0 ..] lights, switches)
        , parts =
            ( sum . map \(lights, switches) ->
                maybe (error "no solution") length
                    . firstJust (firstJust (\(s, ls) -> guard (allOff ls) $> s))
                    $ flip iterate [([], lights)] \ls ->
                        concatMap (\s -> map (\(ss, l) -> (s : ss, applySwitch s l)) ls) switches
            )
                /\ nil
        , extraTests = mempty
        }

data Light = On | Off
    deriving (Eq, Ord, Show, Generic, NFData)
flipLight :: Light -> Light
flipLight = \case
    On -> Off
    Off -> On

newtype Lights = Lights (IM.IntMap Light)
    deriving (Eq, Ord, Show)
    deriving newtype (NFData)
allOff :: Lights -> Bool
allOff (Lights ls) = all (== Off) $ map snd $ IM.toList ls

newtype Switch = Switch [Int]
    deriving (Eq, Ord, Show)
    deriving newtype (NFData)
applySwitch :: Switch -> Lights -> Lights
applySwitch (Switch ss) (Lights ls) = Lights $ foldl' (flip $ IM.adjust flipLight) ls ss
