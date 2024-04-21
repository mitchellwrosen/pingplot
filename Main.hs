module Main (main) where

import Control.Concurrent.STM
import Control.Foldl qualified as Foldl
import Control.Monad (forever)
import Data.Bits (unsafeShiftL)
import Data.Foldable (fold)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO (hGetLine)
import Data.Text.Read qualified as Text
import Data.Void (Void)
import GHC.Clock (getMonotonicTime)
import Ki qualified
import System.Exit (exitFailure)
import System.Process
import Termbox.Tea
import Text.Printf (printf)
import Utils.Containers.Internal.BitUtil (highestBitMask)
import Witherable (catMaybes)

main :: IO ()
main = do
  pongQueue <- newTQueueIO

  Ki.scoped \scope -> do
    Ki.fork_ scope (runPingThread pongQueue)
    tuiMain pongQueue

runPingThread :: TQueue Pong -> IO Void
runPingThread pongQueue =
  withCreateProcess config \_ stdout _ _ ->
    forever do
      line <- hGetLine (fromJust stdout)
      timestamp <- getMonotonicTime
      whenJust (parsePong timestamp line) \pong -> do
        atomically (writeTQueue pongQueue pong)
  where
    config :: CreateProcess
    config =
      (shell "ping -i 0.25 -Q 8.8.8.8") {std_err = NoStream, std_out = CreatePipe}

tuiMain :: TQueue Pong -> IO ()
tuiMain pongQueue = do
  let initialize :: Size -> S
      initialize size =
        S
          { done = False,
            pongs = Seq.empty,
            size
          }

  let pollEvent :: Maybe (IO Pong)
      pollEvent =
        Just (atomically (readTQueue pongQueue))

  let handleEvent :: S -> Event Pong -> IO S
      handleEvent state event = do
        pure case event of
          EventKey key ->
            case key of
              KeyChar 'q' -> state {done = True}
              KeyEsc -> state {done = True}
              _ -> state
          EventResize size -> state {size}
          EventMouse _ -> state
          EventUser pong -> state {pongs = insertPong pong state.pongs}

  let render :: S -> Scene
      render state =
        let barsPos = Pos {row = 2, col = 5}
            barsSize =
              Size
                { width = state.size.width - barsPos.col,
                  height = state.size.height - (2 * barsPos.row)
                }

            -- the second +1 is optional, just favors lower bars
            dottedRow = (barsPos.row + barsSize.height + 2) `div` 2

            buckets = bucketThePongs (.icmp) barsSize.width state.pongs
            buckets1 = summarizeBucket (.milliseconds) <$> buckets

            slowest = summarizeBuckets (\(_, _, v) -> v) buckets1
            -- slowest up to next power of 2, e.g. 117 -> 128
            slowest1 =
              let m = highestBitMask (round @Double @Word slowest)
               in realToFrac @Word @Double (unsafeShiftL m 1)
            vh = max 1 . round @Double @Int . (* i2d barsSize.height) . (/ slowest1)

            rect :: Pos -> Size -> Image -> Image
            rect pos size cell =
              [0 .. size.height - 1] & foldMap \row ->
                [0 .. size.width - 1] & foldMap \col ->
                  cell & at (pos <> Pos {row, col})
         in (image . fold)
              [ -- Draw the axes
                foldMap (\col -> char '┄' & at Pos {row = dottedRow, col}) [0 .. state.size.width - 1],
                let col = barsPos.col - 1
                 in foldMap (\row -> char '│' & at Pos {row, col}) [barsPos.row .. barsPos.row + barsSize.height - 1],
                foldMap (\col -> char '─' & at Pos {row = state.size.height - 2, col}) [0 .. state.size.width - 1],
                (char '┼' & at Pos {row = barsPos.row + barsSize.height, col = barsPos.col - 1}),
                (char '┼' & at Pos {row = dottedRow, col = barsPos.col - 1}),
                printf "% 4.0f┼" slowest1 & zip [0..] & foldMap \(col, c) ->
                  char c & at Pos {row = barsPos.row, col},
                -- Draw the buckets
                ( buckets1 & Foldable.toList & zip [0 ..] & foldMap \(i, (_, x, v)) ->
                    let row = barsPos.row + barsSize.height - height
                        col = barsPos.col + i * width
                        width = 1
                        height = if x == 0 then vh v else 0
                     in rect Pos {row, col} Size {width, height} (char ' ' & bg (if x == 0 then green else yellow))
                          <> ( if row <= dottedRow
                                 then char '┄' & fg (gray 0) & bg green & at Pos {row = dottedRow, col}
                                 else mempty
                             )
                )
              ]

  let finished :: S -> Bool
      finished state =
        state.done

  result <-
    run
      Program
        { initialize,
          pollEvent,
          handleEvent,
          render,
          finished
        }

  case result of
    Left _ -> exitFailure
    Right _ -> pure ()

data S = S
  { done :: !Bool, -- Has the user requested we quit the program?
    pongs :: !(Seq Pong), -- Every pong we've received, ordered by sequence number
    size :: {-# UNPACK #-} !Size -- The size of the terminal
  }
  deriving stock (Show)

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

-- Insert a pong at the end, maintaining sorted icmp sequence number order
insertPong :: Pong -> Seq Pong -> Seq Pong
insertPong pong = \case
  Seq.Empty -> Seq.singleton pong
  xs@(ys Seq.:|> z)
    | pong.icmp > z.icmp -> xs Seq.|> pong
    | otherwise -> insertPong pong ys Seq.|> z

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f =
  maybe (pure ()) f x

-- Summarize a bucket as the total number of pongs, the amount of those that timed out (value 0), and the average of the
-- ones that didn't time out (value more than 0).
summarizeBucket :: (a -> Double) -> Seq a -> (Int, Int, Double)
summarizeBucket getValue =
  Foldl.fold $
    (\c x s -> (c, x, divide s (c - x)))
      <$> Foldl.length
      <*> Foldl.Fold (\acc x -> if getValue x == 0 then acc + 1 else acc) 0 id
      <*> Foldl.Fold (\acc x -> acc + getValue x) 0 id
  where
    divide :: Double -> Int -> Double
    divide _ 0 = 0
    divide s x = s / i2d x

-- Summarize a collection of buckets as the maximum value.
summarizeBuckets :: (a -> Double) -> Seq a -> Double
summarizeBuckets getValue =
  Foldl.fold (Foldl.Fold (\acc x -> max acc (getValue x)) 0 id)

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
        (getSeqnum -> minSeqnum) Seq.:<| (_ Seq.:|> (getSeqnum -> maxSeqnum)) ->
          let bucketSize = realToFrac @Int @Rational (maxSeqnum - minSeqnum + 1) / realToFrac @Int @Rational buckets
           in catMaybes <$> chunksOf bucketSize (fillHoles getSeqnum pongs)
        _ -> undefined

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
