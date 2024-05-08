module Main (main) where

import Control.Concurrent.STM
import Control.Exception (throwIO, try)
import Control.Foldl qualified as Foldl
import Control.Monad (when)
import Data.Bits (unsafeShiftL, unsafeShiftR)
import Data.Foldable (asum, fold)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Functor (($>))
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO.Utf8 qualified as Text
import Data.Text.Read qualified as Text
import GHC.Clock (getMonotonicTime)
import Ki qualified
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO.Error (isEOFError)
import System.Info qualified
import System.Process
import Termbox.Tea
import Text.Printf (printf)
import Utils.Containers.Internal.BitUtil (highestBitMask)
import Witherable (catMaybes)
import Prelude hiding (last)

main :: IO ()
main = do
  pongQueue <- newTQueueIO
  beginning <- getMonotonicTime
  (err, exitCode) <-
    Ki.scoped \scope -> do
      pingThread <- Ki.fork scope (runPingThread pongQueue)
      tuiThread <- Ki.fork scope (tuiMain pongQueue beginning)
      atomically $
        asum $
          [ Ki.await pingThread,
            Ki.await tuiThread $> (Text.empty, ExitSuccess)
          ]
  when (not (Text.null err)) (Text.putStr err)
  exitWith exitCode

runPingThread :: TQueue Pong -> IO (Text, ExitCode)
runPingThread pongQueue =
  withCreateProcess config \_ stdout stderr processHandle ->
    Ki.scoped \scope -> do
      let getln handle =
            try @IOError (Text.hGetLine handle) >>= \case
              Left ex | isEOFError ex -> pure Nothing
              Left ex -> throwIO ex
              Right line -> pure (Just line)
      stdoutThread <-
        Ki.fork scope do
          let loop = do
                getln (fromJust stdout) >>= \case
                  Nothing -> pure ()
                  Just line -> do
                    timestamp <- getMonotonicTime
                    whenJust (parsePong timestamp line) \pong ->
                      atomically (writeTQueue pongQueue pong)
                    loop
          loop
      stderrThread <- Ki.fork scope (Text.hGetContents (fromJust stderr))
      exitCode <- waitForProcess processHandle
      atomically (Ki.await stdoutThread)
      err <- atomically (Ki.await stderrThread)
      pure (err, exitCode)
  where
    config :: CreateProcess
    config =
      -- don't change 4 pings per second without adjusting [pings-per-second] elsewhere
      (shell command) {std_err = CreatePipe, std_out = CreatePipe}
      where
        command =
          case System.Info.os of
            "darwin" -> "ping -i 0.25 -Q 8.8.8.8"
            _ -> "ping -i 0.25 8.8.8.8"

tuiMain :: TQueue Pong -> Double -> IO ()
tuiMain pongQueue beginning = do
  let handleEvent :: State -> Event Pong -> IO State
      handleEvent state event = do
        pure case event of
          EventKey key ->
            case key of
              KeyArrowLeft ->
                state
                  { xrange =
                      case state.xrange of
                        Nothing ->
                          let chunksOf30 = fst (properFraction ((state.timestamp - beginning) / 30))
                           in if chunksOf30 == 0 then Nothing else Just (chunksOf30 * 30)
                        Just n -> Just (max 30 (n - 30))
                  }
              KeyArrowRight ->
                state
                  { xrange =
                      case state.xrange of
                        Nothing -> Nothing
                        Just n ->
                          let n1 = n + 30
                           in if i2d n1 > state.timestamp - beginning then Nothing else Just n1
                  }
              KeyArrowDown -> state {ymax = if state.ymax > 16 then unsafeShiftR state.ymax 1 else state.ymax}
              KeyArrowUp -> state {ymax = if state.ymax < 32768 then unsafeShiftL state.ymax 1 else state.ymax}
              KeyChar 'q' -> state {done = True}
              KeyChar 's' -> state {smoothLevel = cycleSmoothLevel state.smoothLevel}
              KeyEsc -> state {done = True}
              _ -> state
          EventResize size -> state {size}
          EventMouse _ -> state
          EventUser pong ->
            state
              { pongs = insertPong pong state.pongs,
                timestamp = pong.timestamp
              }

  result <-
    run
      Program
        { initialize = \size ->
            State
              { done = False,
                pongs = Seq.empty,
                size,
                smoothLevel = SmoothLevel0,
                timestamp = beginning,
                xrange = Nothing,
                ymax = 32
              },
          pollEvent = Just (atomically (readTQueue pongQueue)),
          handleEvent,
          render = renderState beginning,
          finished = (.done)
        }

  case result of
    Left _ -> exitFailure
    Right _ -> pure ()

data State = State
  { done :: !Bool, -- Has the user requested we quit the program?
    pongs :: !(Seq Pong), -- Every pong we've received, ordered by sequence number
    size :: {-# UNPACK #-} !Size, -- The size of the terminal
    smoothLevel :: !SmoothLevel, -- How much to smooth?
    timestamp :: {-# UNPACK #-} !Double, -- The current monotonic time
    xrange :: {-# UNPACK #-} !(Maybe Int), -- The latest number of seconds, or everything
    ymax :: {-# UNPACK #-} !Word -- The maximum y value on the graph, a power of 2
  }
  deriving stock (Show)

data SmoothLevel
  = SmoothLevel0
  | SmoothLevel1
  | SmoothLevel2
  deriving stock (Show)

cycleSmoothLevel :: SmoothLevel -> SmoothLevel
cycleSmoothLevel = \case
  SmoothLevel0 -> SmoothLevel1
  SmoothLevel1 -> SmoothLevel2
  SmoothLevel2 -> SmoothLevel0

renderState :: Double -> State -> Scene
renderState beginning state =
  let barsPos =
        Pos
          { row = 2,
            col = state.size.width - barsSize.width
          }

      barsSize =
        Size
          { width =
              let maxWidth =
                    state.size.width
                      - if
                        | state.ymax < 100 -> 3
                        | state.ymax < 1000 -> 4
                        | state.ymax < 10000 -> 5
                        | otherwise -> 6
               in min maxWidth (Seq.length pongs),
            height = state.size.height - 5
          }

      -- the first +1 is for correctness, and the second is is optional, it just favors lower bars
      oneHalfRow = (barsPos.row + barsSize.height + 2) `div` 2
      oneQuarterRow = (barsPos.row + barsSize.height + 2) * 3 `div` 4
      oneEighthRow = (barsPos.row + barsSize.height + 2) * 7 `div` 8
      dottedRows = [oneHalfRow, oneQuarterRow, oneEighthRow]

      -- Maybe throw away some old pongs per xrange. This is just approximate since pongs are in icmp order, not
      -- timestamp order. If we want the latest 30 seconds, we just take the last 30*[pings-per-second]=120 pongs from
      -- the end
      pongs =
        case state.xrange of
          Nothing -> state.pongs
          Just xrange -> Seq.drop (Seq.length state.pongs - (xrange * 4)) state.pongs

      -- Bucket all the pongs per the number of bars we can show
      buckets = bucketThePongs (.icmp) barsSize.width pongs

      -- Summarize each bucket
      summaries0 = summarizeBucket (.milliseconds) <$> buckets

      -- Smooth each summary a bit by its neighbors (so the UI is less jumpy as values move between buckets)
      summaries =
        let smooth =
              case state.smoothLevel of
                SmoothLevel0 -> id
                SmoothLevel1 -> smoothBuckets3 (.averagePong) (\averagePong summary -> summary {averagePong})
                SmoothLevel2 -> smoothBuckets5 (.averagePong) (\averagePong summary -> summary {averagePong})
         in smooth summaries0

      _slowest = summarizeBuckets (.averagePong) summaries

      -- height of a value, ignoring that we may be zoomed in too far
      vh = (`divMod` 8) . max 1 . round @Double @Int . (* 8) . (* i2d barsSize.height) . (/ w2d state.ymax)

      rect :: Pos -> Size -> Image -> Image
      rect pos size cell =
        [0 .. size.height - 1] & foldMap \row ->
          [0 .. size.width - 1] & foldMap \col ->
            cell & at (pos <> Pos {row, col})
   in (image . fold)
        [ -- Draw the dotted rows all the way across
          dottedRows & foldMap \row ->
            [barsPos.col .. barsPos.col + barsSize.width - 1] & foldMap \col ->
              char '┄' & at Pos {row, col},
          -- Draw the Y axis
          let col = barsPos.col - 1
           in fold
                [ [barsPos.row .. barsPos.row + barsSize.height - 1] & foldMap \row -> char '│' & at Pos {row, col},
                  dottedRows & foldMap \row -> char '┼' & at Pos {row, col}
                ],
          -- Draw the X axis
          let row = barsPos.row + barsSize.height
           in [barsPos.col .. barsPos.col + barsSize.width - 1] & foldMap \col -> char '─' & at Pos {row, col},
          -- Draw the corner
          char '├' & at Pos {row = barsPos.row + barsSize.height, col = barsPos.col - 1},
          -- Draw the top-left label
          printf "%4d┼" state.ymax & zip [0 ..] & foldMap \(i, c) ->
            let col = barsPos.col - 5 + i in char c & at Pos {row = barsPos.row, col},
          -- Draw the bottom-left label
          let row = barsPos.row + barsSize.height + 1
              (m, s) =
                let s0 =
                      case state.xrange of
                        Nothing -> state.timestamp - beginning
                        Just xrange -> i2d xrange
                 in properFraction (s0 / 60)
           in printf "%d:%02.0f ago" (m :: Int) (s * 60) & zip [0 ..] & foldMap \(i, c) ->
                let col = barsPos.col - 1 + i in char c & at Pos {row, col},
          -- Draw the buckets
          summaries & Foldable.toList & zip [0 ..] & foldMap \(i, summary) ->
            let pos =
                  Pos
                    { row = barsPos.row + barsSize.height - size.height,
                      col = barsPos.col + i * size.width
                    }
                (idealHeight, cap) =
                  if summary.numTimedOutPongs == 0
                    then vh summary.averagePong
                    else (0, 0)
                size =
                  Size
                    { width = 1,
                      height = min idealHeight barsSize.height
                    }
                capi =
                  case cap of
                    1 -> char '▁'
                    2 -> char '▂'
                    3 -> char '▃'
                    4 -> char '▄'
                    5 -> char '▅'
                    6 -> char '▆'
                    7 -> char '▇'
                    _ -> mempty
                clipping = (idealHeight > size.height) || (idealHeight == barsSize.height && cap > 0)
             in if clipping
                  then
                    fold
                      [ rect pos size (char ' ' & bg yellow),
                        dottedRows & foldMap \row ->
                          let col = pos.col in char '┄' & fg (gray 0) & bg yellow & at Pos {row, col}
                      ]
                  else
                    fold
                      [ rect pos size (char ' ' & bg green),
                        capi & fg green & at (pos & posUp 1),
                        dottedRows & foldMap \row ->
                          if pos.row <= row
                            then let col = pos.col in char '┄' & fg (gray 0) & bg green & at Pos {row, col}
                            else mempty
                      ]
                      -- Draw some temporary boundaries just to see stuff
                      -- [0, 1, state.size.height - 2, state.size.height - 1] & foldMap \row ->
                      --   [0 .. state.size.width - 1] & foldMap \col ->
                      --     char ' ' & bg (gray 6) & at Pos {row, col}
        ]

data Pong = Pong
  { timestamp :: {-# UNPACK #-} !Double, -- monotonic time this pong was received
    icmp :: {-# UNPACK #-} !Int, -- ping sequence number
    milliseconds :: {-# UNPACK #-} !Double -- 0 means timed out
  }
  deriving stock (Show)

parsePong :: Double -> Text -> Maybe Pong
parsePong timestamp line
  | "64 bytes from 8.8.8.8" `Text.isPrefixOf` line = do
      [icmp0, _ttl, milliseconds0, "ms"] <- Just (Text.words (Text.drop 23 line))
      icmp1 <- Text.stripPrefix "icmp_seq=" icmp0
      Right (icmp, "") <- Just (Text.decimal icmp1)
      milliseconds1 <- Text.stripPrefix "time=" milliseconds0
      Right (milliseconds, "") <- Just (Text.double milliseconds1)
      Just Pong {timestamp, icmp, milliseconds}
  | "Request timeout" `Text.isPrefixOf` line = do
      Right (icmp, "") <- Just (Text.decimal (Text.takeWhileEnd (/= ' ') line))
      Just Pong {timestamp, icmp, milliseconds = 0}
  | otherwise = Nothing

-- Insert a pong at the end, maintaining sorted icmp sequence number order. We may get duplicate icmp pongs, e.g.
-- packet 68 timed out, then we got packet 68 back. That's kinda weird, but the ping executable just works like that?
-- So, let's just replace whatever's there, if anything.
insertPong :: Pong -> Seq Pong -> Seq Pong
insertPong pong = \case
  Seq.Empty -> Seq.singleton pong
  xs@(ys Seq.:|> z) ->
    case compare pong.icmp z.icmp of
      LT -> insertPong pong ys Seq.|> z
      EQ -> ys Seq.|> pong -- overwrite existing pong with same sequence number
      GT -> xs Seq.|> pong

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f =
  maybe (pure ()) f x

data BucketSummary = BucketSummary
  { numPongs :: {-# UNPACK #-} !Int, -- total number of pongs, including timed-out ones
    numTimedOutPongs :: {-# UNPACK #-} !Int, -- total number of timed-out pongs
    averagePong :: {-# UNPACK #-} !Double -- average value of non-timed-out pongs
  }

-- Summarize a bucket as the total number of pongs, the amount of those that timed out (value 0), and the average of the
-- ones that didn't time out (value more than 0).
summarizeBucket :: (a -> Double) -> Seq a -> BucketSummary
summarizeBucket getValue =
  Foldl.fold (summarizeBucketFold getValue)

summarizeBucketFold :: (a -> Double) -> Foldl.Fold a BucketSummary
summarizeBucketFold getValue =
  ( \numPongs numTimedOutPongs pongTotal ->
      BucketSummary
        { numPongs,
          numTimedOutPongs,
          averagePong = pongTotal `divide` (numPongs - numTimedOutPongs)
        }
  )
    <$> Foldl.length
    <*> Foldl.Fold (\acc x -> if getValue x == 0 then acc + 1 else acc) 0 id
    <*> Foldl.Fold (\acc x -> acc + getValue x) 0 id
  where
    divide :: Double -> Int -> Double
    divide _ 0 = 0
    divide s x = s / i2d x

smoothBuckets3 :: forall a. (a -> Double) -> (Double -> a -> a) -> Seq a -> Seq a
smoothBuckets3 getValue setValue =
  smooth3
    (\() me () -> me)
    (\() me z -> on2 z me)
    (\x me () -> on2 x me)
    (\x me z -> setValue ((getValue x + getValue me + getValue z) / 3) me)
  where
    on2 x me = setValue ((getValue x + getValue me) / 2) me

smoothBuckets5 :: forall a. (a -> Double) -> (Double -> a -> a) -> Seq a -> Seq a
smoothBuckets5 getValue setValue =
  smooth5
    (\() () me () () -> me)
    (\() () me a () -> on2 a me)
    (\() a me () () -> on2 a me)
    (\() () me a b -> on3 a b me)
    (\() a me b () -> on3 a b me)
    (\a b me () () -> on3 a b me)
    (\() a me b c -> on4 a b c me)
    (\a b me c () -> on4 a b c me)
    (\a b me c d -> on5 a b c d me)
  where
    on2 a me = setValue (summy [a, me] / 2) me
    on3 a b me = setValue (summy [a, b, me] / 3) me
    on4 a b c me = setValue (summy [a, b, c, me] / 4) me
    on5 a b c d me = setValue (summy [a, b, c, d, me] / 5) me

    summy :: [a] -> Double
    summy = List.foldl' (\acc x -> acc + getValue x) 0

-- Smooth a sequence by applying a function to an element and its surrounding values.
--
-- >>> smooth3 (\() y () -> y) (\() y z -> y) (\x y () -> y) (\x y z -> x + y + z) (Seq.fromList [1,2,3,4,5])
-- fromList [1,6,9,12,5]
--
-- >>> smooth3 (\() y () -> False) (\() y z -> True) (\x y () -> True) (\x y z -> True) (Seq.fromList [1])
-- fromList [False]
smooth3 ::
  (() -> a -> () -> b) ->
  (() -> a -> a -> b) ->
  (a -> a -> () -> b) ->
  (a -> a -> a -> b) ->
  Seq a ->
  Seq b
smooth3 f1 f2 f3 f4 = \case
  Seq0 -> Seq.empty
  Seq1 x -> Seq.singleton (f1 () x ())
  Seq2p x y ys -> go (Seq.singleton (f2 () x y)) x y ys
  where
    go acc x y = \case
      Seq.Empty -> acc Seq.|> f3 x y ()
      z Seq.:<| zs -> go (acc Seq.|> f4 x y z) y z zs

-- Smooth a sequence by applying a function to an element and its surrounding values.
smooth5 ::
  forall a b.
  (() -> () -> a -> () -> () -> b) ->
  (() -> () -> a -> a -> () -> b) ->
  (() -> a -> a -> () -> () -> b) ->
  (() -> () -> a -> a -> a -> b) ->
  (() -> a -> a -> a -> () -> b) ->
  (a -> a -> a -> () -> () -> b) ->
  (() -> a -> a -> a -> a -> b) ->
  (a -> a -> a -> a -> () -> b) ->
  (a -> a -> a -> a -> a -> b) ->
  Seq a ->
  Seq b
smooth5 f1 f2 f3 f4 f5 f6 f7 f8 f9 = \case
  Seq0 -> Seq.empty
  Seq1 a -> Seq.singleton (f1 () () a () ())
  Seq2 a b -> f2 () () a b () Seq.<| Seq.singleton (f3 () a b () ())
  Seq3 a b c -> f4 () () a b c Seq.<| f5 () a b c () Seq.<| Seq.singleton (f6 a b c () ())
  Seq4p a b c d ds -> go (f4 () () a b c Seq.<| Seq.singleton (f7 () a b c d)) a b c d ds
  where
    go :: Seq b -> a -> a -> a -> a -> Seq a -> Seq b
    go acc a b c d = \case
      Seq.Empty -> acc Seq.|> f8 a b c d ()
      e Seq.:<| es -> go (acc Seq.|> f9 a b c d e) b c d e es

-- Exactly 0 elements
pattern Seq0 :: Seq a
pattern Seq0 <- Seq.Empty

-- Exactly 1 element
pattern Seq1 :: a -> Seq a
pattern Seq1 a <- a Seq.:<| Seq0

-- Exactly 2 elements
pattern Seq2 :: a -> a -> Seq a
pattern Seq2 a b <- Seq2p a b Seq0

-- 2+ elements
pattern Seq2p :: a -> a -> Seq a -> Seq a
pattern Seq2p a b bs <- a Seq.:<| b Seq.:<| bs

-- Exactly 3 elements
pattern Seq3 :: a -> a -> a -> Seq a
pattern Seq3 a b c <- Seq3p a b c Seq0

-- 3+ elements
pattern Seq3p :: a -> a -> a -> Seq a -> Seq a
pattern Seq3p a b c cs <- a Seq.:<| Seq2p b c cs

-- 4+ elements
pattern Seq4p :: a -> a -> a -> a -> Seq a -> Seq a
pattern Seq4p a b c d ds <- a Seq.:<| Seq3p b c d ds

{-# COMPLETE Seq0, Seq1, Seq2p #-}

{-# COMPLETE Seq0, Seq1, Seq2, Seq3, Seq4p #-}

-- Summarize a collection of buckets as the maximum value.
summarizeBuckets :: (a -> Double) -> Seq a -> Double
summarizeBuckets getValue =
  Foldl.fold (summarizeBucketsFold getValue)

summarizeBucketsFold :: (a -> Double) -> Foldl.Fold a Double
summarizeBucketsFold getValue =
  Foldl.Fold (\acc x -> max acc (getValue x)) 0 id

-- Bucket an array of sequence-ordered things (possibly with holes):
--
--   1. Find out how many pongs go in each bucket, based on the difference in sequence number between the first and
--      last pong in the sequence. For example, if the first pong has sequence number 0 (likely), and the last pong has
--      sequence number 14 (so there would be 15 pongs total if no holes), and we want 6 buckets, then each bucket holds
--      15/6 pongs.
--   2. Map over the sequence and fill any gaps in sequence number with Nothing values.
--   3. Put 15/6 pongs/holes in each bucket, rounded to the nearest integer of course (carrying fractional parts over
--      to the next hole).
--   4. Throw away the hole Nothing values.
--
-- >>> bucketThePongs id 3 (Seq.fromList [1,2,3,5,6,7])
-- fromList [fromList [1,2],fromList [3],fromList [5,6,7]]
bucketThePongs :: (a -> Int) -> Int -> Seq a -> Seq (Seq a)
bucketThePongs getSeqnum buckets pongs
  | buckets <= 0 = Seq.empty
  | buckets == 1 = Seq.singleton pongs
  | buckets >= Seq.length pongs = Seq.singleton <$> pongs
  -- below here we know that buckets >= 2, and length pongs >= 3
  | otherwise =
      case pongs of
        first Seq.:<| (_ Seq.:|> last) ->
          let bucketSize = rat (getSeqnum last - getSeqnum first + 1) / rat buckets
           in catMaybes <$> chunksOf bucketSize (fillHoles getSeqnum pongs)
        _ -> undefined
  where
    rat = realToFrac @Int @Rational

-- >>> fillHoles id (Seq.fromList [4,7,8,10])
-- fromList [Just 4,Nothing,Nothing,Just 7,Just 8,Nothing,Just 10]
fillHoles :: forall a. (a -> Int) -> Seq a -> Seq (Maybe a)
fillHoles getSeqnum = \case
  Seq.Empty -> Seq.empty
  x Seq.:<| xs -> go (Seq.singleton (Just x)) (getSeqnum x + 1) xs
  where
    go :: Seq (Maybe a) -> Int -> Seq a -> Seq (Maybe a)
    go !acc !next = \case
      Seq.Empty -> acc
      y Seq.:<| ys ->
        let yn = getSeqnum y
            acc1 =
              case yn - next of
                0 -> acc
                n -> acc <> Seq.replicate n Nothing
         in go (acc1 Seq.:|> Just y) (yn + 1) ys

-- >>> Foldable.toList (fmap Foldable.toList (chunksOf 1.2 (Seq.fromList [1,2,3,4,5,6,7,8,9,10])))
-- [[1],[2],[3],[4],[5,6],[7],[8],[9],[10]]
chunksOf :: Rational -> Seq a -> Seq (Seq a)
chunksOf n
  | n <= 0 = const Seq.empty
  | otherwise = go Seq.empty 0.0
  where
    go :: Seq (Seq a) -> Rational -> Seq a -> Seq (Seq a)
    go !acc !excess !xs =
      let (n1, excess1) = properFraction (n + excess)
       in case Seq.splitAt n1 xs of
            (Seq.Empty, _) -> acc
            (ys, zs) -> go (acc Seq.:|> ys) excess1 zs

i2d :: Int -> Double
i2d = realToFrac

w2d :: Word -> Double
w2d = realToFrac

-- Round up to the next power of 2.
--
-- >>> _roundUp2 117
-- 128
--
-- >>> _roundUp2 128
-- 128
_roundUp2 :: Double -> Word
_roundUp2 n
  | w1 == w2 = w2
  | otherwise = unsafeShiftL w2 1
  where
    w1 = round @Double @Word n
    w2 = highestBitMask w1
