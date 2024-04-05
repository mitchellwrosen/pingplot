module Main (main) where

import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO (hGetLine)
import Data.Text.Read qualified as Text
import GHC.Clock (getMonotonicTime)
import Ki qualified
import System.Exit (exitFailure)
import System.IO (Handle)
import System.Process
import Termbox.Tea
import Text.Printf (printf)

main :: IO ()
main = do
  pongQueue <- newTQueueIO

  Ki.scoped \scope -> do
    Ki.fork_ scope do
      withPingProcess \stdout ->
        forever do
          line <- hGetLine stdout
          timestamp <- getMonotonicTime
          whenJust (parsePong timestamp line) \pong -> do
            atomically (writeTQueue pongQueue pong)

    tuiMain pongQueue

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
        let pongs = Foldable.toList (Seq.drop (Seq.length state.pongs - state.size.width) state.pongs)
            npongs = min (Seq.length state.pongs) state.size.width
            slowest =
              List.foldl'
                (\acc pong -> max acc pong.milliseconds)
                0
                pongs
            msHeight ms = max 1 (round @Double @Int ((ms / slowest) * realToFrac @Int @Double state.size.height))
         in image
              ( ( pongs & zip [0 ..] & foldMap \(i, pong) ->
                    let col = state.size.width - npongs + i
                        height =
                          case pong.milliseconds of
                            0 -> 0
                            ms -> msHeight ms
                     in [state.size.height - height .. state.size.height - 1] & foldMap \row ->
                          char ' '
                            & bg green
                            & at (Pos row col)
                )
                  <> foldMap (\(i, c) -> char c & atCol i) (zip [0 ..] (printf "%.0fms" slowest))
              )

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
  { done :: !Bool,
    pongs :: !(Seq Pong),
    size :: !Size
  }
  deriving stock (Show)

withPingProcess :: (Handle -> IO a) -> IO a
withPingProcess action =
  withCreateProcess ((shell "ping -Q 8.8.8.8") {std_err = NoStream, std_out = CreatePipe}) \_ stdout _ _ ->
    action (fromJust stdout)

data Pong = Pong
  { timestamp :: !Double, -- monotonic time this pong was received
    icmp :: !Int, -- ping sequence number
    milliseconds :: !Double
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
