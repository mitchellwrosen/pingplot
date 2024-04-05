module Main (main) where

import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO (hGetLine)
import Data.Text.Read qualified as Text
import GHC.Clock
import Ki qualified
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.Exit (exitFailure)
import System.IO (Handle)
import System.Process
import Termbox.Tea
import Text.Printf
import qualified Data.Foldable as Foldable

main :: IO ()
main = do
  pongQueue <- newTQueueIO

  Ki.scoped \scope -> do
    Ki.fork_ scope do
      withPingProcess \stdout ->
        forever do
          line <- hGetLine stdout
          whenJust (parsePongTime line) \time -> do
            timestamp <- getMonotonicTime
            atomically (writeTQueue pongQueue Pong {timestamp, time})

    tuiMain pongQueue

tuiMain :: TQueue Pong -> IO ()
tuiMain pongQueue = do
  let initialize :: Size -> S
      initialize size =
        S
          { done = False,
            numPongs = 0,
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
          EventUser pong ->
            state
              { numPongs = state.numPongs + 1,
                pongs = state.pongs Seq.|> pong
              }

  let render :: S -> Scene
      render state =
        let pongs = Foldable.toList (Seq.drop (state.numPongs - state.size.width) state.pongs)
            npongs = min state.numPongs state.size.width
            slowest =
              List.foldl'
                ( \acc pong ->
                    case pong.time of
                      PongTime ms -> max acc ms
                      PongTimeout -> acc
                )
                0
                pongs
            msHeight ms = round @Double @Int ((ms / slowest) * realToFrac @Int @Double state.size.height)
         in image
              ( ( pongs & zip [0 ..] & foldMap \(i, pong) ->
                    let col = state.size.width - npongs + i
                        height =
                          case pong.time of
                            PongTime ms -> msHeight ms
                            PongTimeout -> 0
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
    numPongs :: !Int,
    pongs :: !(Seq Pong),
    size :: !Size
  }
  deriving stock (Show)

data Pong = Pong
  { timestamp :: !Double,
    time :: !PongTime
  }
  deriving stock (Show)

withPingProcess :: (Handle -> IO a) -> IO a
withPingProcess action =
  withCreateProcess ((shell "ping -Q 8.8.8.8") {std_err = NoStream, std_out = CreatePipe}) \_ stdout _ _ ->
    action (fromJust stdout)

data PongTime
  = PongTime !Double
  | PongTimeout
  deriving stock (Show)

parsePongTime :: Text -> Maybe PongTime
parsePongTime line
  | Right (ms, _) <- Text.double (Text.takeWhileEnd (/= '=') line) = Just (PongTime ms)
  | "Request timeout" `Text.isPrefixOf` line = Just PongTimeout
  | otherwise = Nothing

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust x f =
  maybe (pure ()) f x
