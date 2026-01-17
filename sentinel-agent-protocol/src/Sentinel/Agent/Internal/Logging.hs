-- |
-- Module      : Sentinel.Agent.Internal.Logging
-- Description : Internal logging utilities
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : internal
--
-- Simple logging utilities for the agent SDK.
-- Uses co-log-core for structured logging with minimal overhead.
module Sentinel.Agent.Internal.Logging
  ( -- * Log Actions
    LogAction (..)
  , LogMessage (..)
  , Severity (..)

    -- * Creating Loggers
  , nullLogger
  , stdoutLogger
  , stderrLogger

    -- * Logging Functions
  , logDebug
  , logInfo
  , logWarn
  , logError

    -- * Utilities
  , withContext
  ) where

import Colog.Core (LogAction (..), (<&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import System.IO (stderr, stdout)
import UnliftIO (MonadIO, liftIO)

-- | Log severity levels
data Severity
  = Debug
  | Info
  | Warn
  | Error
  deriving stock (Eq, Ord, Show, Bounded, Enum)

-- | Structured log message
data LogMessage = LogMessage
  { lmSeverity :: !Severity
  , lmMessage :: !Text
  , lmContext :: ![(Text, Text)]
  }
  deriving stock (Eq, Show)

-- | Format a log message for output
formatLogMessage :: LogMessage -> IO Text
formatLogMessage LogMessage {..} = do
  timestamp <- getCurrentTime
  let severityStr = case lmSeverity of
        Debug -> "DEBUG"
        Info -> "INFO"
        Warn -> "WARN"
        Error -> "ERROR"
      contextStr =
        if null lmContext
          then ""
          else " " <> T.intercalate " " [k <> "=" <> v | (k, v) <- lmContext]
  pure $ T.pack (show timestamp) <> " [" <> severityStr <> "] " <> lmMessage <> contextStr

-- | Logger that discards all messages
nullLogger :: LogAction IO LogMessage
nullLogger = LogAction $ \_ -> pure ()

-- | Logger that writes to stdout
stdoutLogger :: Severity -> LogAction IO LogMessage
stdoutLogger minSeverity = LogAction $ \msg ->
  if lmSeverity msg >= minSeverity
    then do
      formatted <- formatLogMessage msg
      TIO.hPutStrLn stdout formatted
    else pure ()

-- | Logger that writes to stderr
stderrLogger :: Severity -> LogAction IO LogMessage
stderrLogger minSeverity = LogAction $ \msg ->
  if lmSeverity msg >= minSeverity
    then do
      formatted <- formatLogMessage msg
      TIO.hPutStrLn stderr formatted
    else pure ()

-- | Log a debug message
logDebug :: MonadIO m => LogAction IO LogMessage -> Text -> m ()
logDebug logger msg = liftIO $ logger <& LogMessage Debug msg []

-- | Log an info message
logInfo :: MonadIO m => LogAction IO LogMessage -> Text -> m ()
logInfo logger msg = liftIO $ logger <& LogMessage Info msg []

-- | Log a warning message
logWarn :: MonadIO m => LogAction IO LogMessage -> Text -> m ()
logWarn logger msg = liftIO $ logger <& LogMessage Warn msg []

-- | Log an error message
logError :: MonadIO m => LogAction IO LogMessage -> Text -> m ()
logError logger msg = liftIO $ logger <& LogMessage Error msg []

-- | Add context to a logger
withContext :: [(Text, Text)] -> LogAction IO LogMessage -> LogAction IO LogMessage
withContext ctx (LogAction f) =
  LogAction $ \msg -> f msg {lmContext = lmContext msg <> ctx}
