module Main (main) where

import Pre

import Data.Bits
import Data.Time

import Data.Finite
import Data.Functor.Contravariant
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.IO qualified as TL
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
import System.Console.Terminal.Size qualified as Terminal.Size

main :: IO ()
main = do
    -- terminalWidth <- Terminal.Size.width <<$>> Terminal.Size.size -- TODO this doesn't work in GHCID or GHCIWatch...
    -- terminalWidth <- Terminal.Size.width <<$>> sizeFromEnv
    terminalWidth <- pure $ Just 62
    T.putStrLn $ drawTree $ getTestTree tests
    -- TL.putStrLn . displayTestResultsConsole (intToFilledBar . testDurationBarFunction) terminalWidth
    TL.putStrLn . displayTestResultsConsole testDurationBarFunctionSimple terminalWidth
        -- =<< runTests
        --     TestRunnerOpts
        --         { regenerateGoldenFiles = False
        --         -- { regenerateGoldenFiles = True
        --         }
        --     ()
        --     tests
        =<< pure exampleTestResult

testDurationBarFunctionSimple :: NominalDiffTime -> Char
testDurationBarFunctionSimple =
    -- this is pretty nice I think - starts showing at 1ms
    -- but there is a lot for AoC in repl on Crow that's only just under that threshold
    -- also, we don't get anywhere near the top bars in practice
    -- basic 0.001 0.01 0.1 1 10 100 1000 10000
    -- this is a bit less principled (though more so if we used √10), but probably the one - nice range
    basic 0.01 0.03 0.1 0.3 1 3 10 30

-- TODO we want a smooth monotonically-increasing function
-- do we want to aim for it to produce an even spread of values across the output range, for the test times?
-- well, probably not - we'd prefer more at the bottom so the top ones stand out
-- this is going to take some experimentation
-- and it's gonna be hard to know up front really
-- but we might just have to make a best guess, since we want to be able to print lines without running all tests
-- and tbh, the output here really shouldn't depend on which tests are filtered out
-- so it pretty much has to be specified up front
-- what's important then is that it's easily configurable
-- also, I should actually graph these properly rather than relying on GPT and my test output times...
-- and fix `intToFilledBar` as suggested in its comment, before experimenting further here
-- EDIT: actually tbh, given the ultimate output set we care about only has 8 elements, I'm probably overengineering
-- I increasingly think my final version of this should just be `testDurationBarFunctionSimple`
-- along with configurability of course
-- there's a temptation in that case to just do some maths and calculate the character code points
-- but then we probably want `basic` (renamed...) for users anyway, to help with configuration
testDurationBarFunction :: NominalDiffTime -> Word
testDurationBarFunction =
    round
        . (\x -> m * (1 - b ** (-k * x)) ** p)
        -- haven't even tried these two:
        -- . (\x -> m * (x ** p) / (x ** p + c))
        -- . (\x -> m * (1 - b ** (-k * x)) / (1 - b ** (-k * x) + c))
        . realToFrac @_ @Double
  where
    p = 1
    b = 10
    k = 3
    m = fromIntegral @Word @Double maxBound

-- 0 for 0 only, then 1 for the first 8th of the range, etc.
-- TODO actually, maybe we should save the full bar for `maxBound` only instead, and use empty for full lower eighth
-- since having full bars makes the output look a bit weird, as there's then no gap with the line above
-- or even make it unreachable? that would maybe be a bit too inflexible, and max may never be hit in practice anyway
-- definitely make the change but do it in a separate commit, as I'd like it to be easy to revert
intToFilledBar :: Word -> Char
intToFilledBar =
    basic
        -- this might seem a bit unnecessarily low-level, but it's to ensure that it doesn't only work on 64-bit platforms
        (0x0 `rotateR` 4 `xor` 1)
        (0x2 `rotateR` 4 `xor` 1)
        (0x4 `rotateR` 4 `xor` 1)
        (0x6 `rotateR` 4 `xor` 1)
        (0x8 `rotateR` 4 `xor` 1)
        (0xa `rotateR` 4 `xor` 1)
        (0xc `rotateR` 4 `xor` 1)
        (0xe `rotateR` 4 `xor` 1)

-- maxBound

-- first argument is the threshold (inclusive, naturally) at which we start showing one bar
-- second is where we show two bars
-- etc.
basic :: (Ord t) => t -> t -> t -> t -> t -> t -> t -> t -> t -> Char
basic t1 t2 t3 t4 t5 t6 t7 t8 t
    | t < t1 = ' '
    | t < t2 = '▁'
    | t < t3 = '▂'
    | t < t4 = '▃'
    | t < t5 = '▄'
    | t < t6 = '▅'
    | t < t7 = '▆'
    | t < t8 = '▇'
    | otherwise = '█'

tests :: TestTree IO ()
tests =
    test "tests" pure $
        enumerate <&> \isRealData@(bool "examples" "real" -> t) ->
            test (T.pack t) pure $
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
                    <&> \Puzzle{number = show -> pt, parser, parts, extraTests} ->
                        testLazy
                            (T.pack pt)
                            ( \() -> do
                                let fp = "../inputs/" <> t <> "/" <> pt
                                input <-
                                    either (assertFailure . T.pack . ("parse failure: " <>) . errorBundlePretty) pure
                                        . runParser (parser isRealData <* eof) fp
                                        =<< liftIO (T.readFile fp)
                                let (rs, os) =
                                        (lookupHList fst &&& foldHListF (HCons . snd) HNil) $
                                            mapHListF (\((Fanout (f, Op o))) -> (o &&& id) $ f input) parts
                                 in pure (input, rs, os)
                            )
                            $ ( finites <&> \(n@(show . succ @Int . fromIntegral -> nt)) ->
                                    test
                                        (T.pack nt)
                                        (\(_, rs, _) -> golden ("../outputs/" <> t <> "/" <> pt <> "/" <> nt) $ rs n <> "\n")
                                        []
                              )
                                <> let ts = extraTests isRealData ("../outputs/" <> t <> "/" <> pt <> "/extra/")
                                    in if null ts
                                        then []
                                        else [testLazy "extra" (\(input, _, os) -> pure (input, os)) ts]
