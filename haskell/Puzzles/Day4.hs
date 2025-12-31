module Puzzles.Day4 (puzzle) where

import Pre

import Data.Sequence qualified as Seq
import Data.Stream.Infinite qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T

puzzle :: Puzzle
puzzle =
    Puzzle
        { number = 4
        , parser = const $ (some $ asum $ enumerate <&> \t -> char (inToChar t) $> t) `sepEndBy` newline
        , parts =
            ( (\g -> countRolls g - countRolls (removeAccessibleRolls $ findAccessible g))
                . mkGrid
            )
                /\ ( (id &&& generateFrames) . mkGrid
                   , \(g, fs) ->
                        T.show $ countRolls g - countRolls (fst $ S.head $ S.filter (noneAccessible . snd) fs)
                   )
                /\\ nil
        , extraTests = \isRealData path input (HCons _ (HCons (_, fmap snd -> frameStream) HNil)) -> do
            it "round trip" do
                t <- T.readFile if isRealData then "../inputs/real/4" else "../inputs/examples/4"
                drawGrid (mkGrid input <&> \case InEmpty -> OutEmpty; InRoll -> OutRoll) `shouldBe` t
            describe "frames" do
                let frames = Seq.fromList $ takeUntil noneAccessible frameStream
                -- note that `nFrames = Seq.length frames - 1`, but we don't define it as such
                -- since that would force the expensive evaluation during test tree construction, messing up reporting
                let nFrames = if isRealData then 58 else 9
                for_ [0 .. nFrames] \n ->
                    it (show n) . pureGoldenTextFile (path <> "frames/" <> show n) $
                        maybe "frame list too short!" drawGrid (Seq.lookup n frames)
                it "end" do
                    Just g <- pure $ Seq.lookup nFrames frames
                    (g `shouldSatisfyNamed` "accessible tile found") noneAccessible
        }

newtype Grid a = Grid (Seq (Seq (V2 Int, a)))
    deriving (Functor, Show)

data InTile
    = InEmpty
    | InRoll
    deriving (Eq, Ord, Show, Enum, Bounded)
inToChar :: InTile -> Char
inToChar = \case
    InEmpty -> '.'
    InRoll -> '@'

data OutTile
    = OutEmpty
    | OutRoll
    | OutAccessible
    deriving (Eq, Ord, Show, Enum, Bounded)
outToChar :: OutTile -> Char
outToChar = \case
    OutEmpty -> inToChar InEmpty
    OutRoll -> inToChar InRoll
    OutAccessible -> 'x'

drawGrid :: Grid OutTile -> T.Text
drawGrid (Grid g) = T.unlines . toList . fmap (T.pack . toList . fmap outToChar) $ snd <<$>> g

mkGrid :: [[a]] -> Grid a
mkGrid = Grid . Seq.fromList . map Seq.fromList . zipWith (map . first . V2) [0 ..] . map (zip [0 ..])

findAccessible :: Grid InTile -> Grid OutTile
findAccessible (Grid inGrid) =
    Grid $
        inGrid <<&>> \(v, t) -> (v,) case t of
            InEmpty -> OutEmpty
            InRoll ->
                if length (filter ((== Just InRoll) . fmap snd) neighbours) < 4
                    then OutAccessible
                    else OutRoll
              where
                neighbours = do
                    x <- [-1 .. 1]
                    y <- [-1 .. 1]
                    guard $ not (x == 0 && y == 0)
                    let V2 x' y' = v + V2 x y
                    pure $ Seq.lookup x' inGrid >>= Seq.lookup y'

removeAccessibleRolls :: Grid OutTile -> Grid InTile
removeAccessibleRolls = fmap \case
    OutEmpty -> InEmpty
    OutRoll -> InRoll
    OutAccessible -> InEmpty

generateFrames :: Grid InTile -> Stream (Grid InTile, Grid OutTile)
generateFrames = unfoldMutual findAccessible removeAccessibleRolls

noneAccessible :: Grid OutTile -> Bool
noneAccessible (Grid g) = not $ any (elem OutAccessible . fmap snd) g

countRolls :: Grid InTile -> Int
countRolls (Grid g) = length $ concatMap (filter (== InRoll) . toList . fmap snd) g

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap
(<<&>>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)

takeUntil :: (Foldable t) => (a -> Bool) -> t a -> [a]
takeUntil p = foldr (\x xs -> x : if p x then [] else xs) []

unfoldMutual :: (a -> b) -> (b -> a) -> a -> Stream (a, b)
unfoldMutual f g a = let b = f a in (a, b) :> unfoldMutual f g (g b)
