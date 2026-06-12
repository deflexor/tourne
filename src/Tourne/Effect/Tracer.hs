{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Tourne.Effect.Tracer
Description : A small 'Tracer' effect for structured debug logging.

Replaces the @debugEnabled :: Bool@ global + ad-hoc @when debugEnabled@
sprinkling with an effect that an interpreter can choose to honour or
ignore. Two ready-made interpreters:

  * 'runTracer'       -- writes timestamped lines to stderr, gated by
                        a runtime 'Bool' flag.
  * 'runTracerNoop'   -- discards every event (for tests).

Use 'withRunInIO' to extract a plain @Text -> [Text] -> IO ()@
callback for @IO@-only code:

> withRunInIO $ \\runInIO -> do
>   let cb label fields = runInIO (traceEvent label fields)
>   Stream.openStream url cb
-}
module Tourne.Effect.Tracer
  ( -- * Effect
    Tracer
  , traceEvent
    -- * Interpreters
  , runTracer
  , runTracerNoop
  ) where

import Relude hiding (hFlush)
import Data.IORef qualified as IORef
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Effectful (Dispatch (Dynamic), DispatchOf, Effect, Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Internal.Monad (send)
import System.IO (hFlush, hPutStrLn)

-- | A tracer effect. Carries a short label and a list of pre-formatted
-- @key=value@ fields; the interpreter is responsible for the rest of
-- the line format (timestamp, target, etc.).
data Tracer :: Effect where
  TraceEvent :: Text -> [Text] -> Tracer m ()

type instance DispatchOf Tracer = 'Dynamic

-- | Emit a trace event. The interpretation is up to the active
-- interpreter; the no-op interpreter discards it.
traceEvent :: (Tracer :> es) => Text -> [Text] -> Eff es ()
traceEvent label fields = send (TraceEvent label fields)

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------

-- | Write each 'TraceEvent' to @stderr@ as
-- @D <elapsed_ms> <label> <field> <field>...@.
--
-- The 'Bool' flag is checked at each call (cheap), so the same
-- interpreter can be reused if you want to toggle debug at runtime.
-- If you know it's a fixed decision at startup, prefer
-- 'runTracerNoop' when @False@ to avoid the per-call branch.
runTracer
  :: (IOE :> es)
  => IORef.IORef UTCTime  -- ^ start time, written by the audio engine
  -> Bool                -- ^ enable flag
  -> Eff (Tracer : es) a
  -> Eff es a
runTracer startRef enabled =
  interpret_ $ \case
    TraceEvent label fields ->
      if not enabled
        then pure ()
        else do
          start <- liftIO $ IORef.readIORef startRef
          now   <- liftIO getCurrentTime
          let elapsed = round (realToFrac (diffUTCTime now start) * (1000 :: Double)) :: Int
              line   = "D " <> show elapsed <> " " <> toString label
                    <> " " <> toString (Text.intercalate " " fields)
          liftIO $ hPutStrLn stderr line
          liftIO $ hFlush stderr

-- | Discard every 'TraceEvent'. Suitable for tests and the
-- @TOURNE_DEBUG=0@ default.
runTracerNoop :: Eff (Tracer : es) a -> Eff es a
runTracerNoop = interpret_ $ \TraceEvent{} -> pure ()
