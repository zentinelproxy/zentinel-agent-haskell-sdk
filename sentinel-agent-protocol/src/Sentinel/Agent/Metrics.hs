-- |
-- Module      : Sentinel.Agent.Metrics
-- Description : Metrics collection and export for Sentinel agents
-- Copyright   : (c) Raskell, 2026
-- License     : Apache-2.0
-- Maintainer  : agents@raskell.io
-- Stability   : experimental
--
-- This module provides utilities for metrics collection and export
-- in Sentinel agents. Metrics are periodically collected and sent
-- to the proxy for aggregation and monitoring.
--
-- = Metric Types
--
-- * 'CounterMetric' - Monotonically increasing values (e.g., request count)
-- * 'GaugeMetric' - Values that can go up or down (e.g., queue depth)
-- * 'HistogramMetric' - Distribution of values (e.g., latency)
--
-- = Standard Metrics
--
-- The module provides standard metric names that agents should use
-- for consistency:
--
-- * 'metricRequestsTotal' - Total requests processed
-- * 'metricRequestsBlockedTotal' - Total requests blocked
-- * 'metricRequestsDurationSeconds' - Request processing duration
-- * 'metricErrorsTotal' - Total errors encountered
-- * 'metricInFlightRequests' - Current in-flight requests
-- * 'metricQueueDepth' - Current queue depth
--
-- = Usage
--
-- @
-- instance AgentHandler MyAgent where
--   metricsReport = do
--     agent <- ask
--     processed <- readIORef agent.requestsProcessed
--     blocked <- readIORef agent.requestsBlocked
--     timestamp <- getCurrentTimeMs
--     pure $ Just $ newMetricsReport "my-agent-001" timestamp 10000
--       [ counterMetric metricRequestsTotal processed
--       , counterMetric metricRequestsBlockedTotal blocked
--       ]
--       []
--       []
-- @
module Sentinel.Agent.Metrics
  ( -- * Report Builders
    newMetricsReport
  , emptyMetricsReport

    -- * Counter Metrics
  , counterMetric
  , counterWithLabels
  , counterWithHelp

    -- * Gauge Metrics
  , gaugeMetric
  , gaugeWithLabels
  , gaugeWithHelp

    -- * Histogram Metrics
  , histogramMetric
  , histogramWithLabels
  , histogramBucket
  , defaultLatencyBuckets

    -- * Standard Metric Names
  , metricRequestsTotal
  , metricRequestsBlockedTotal
  , metricRequestsDurationSeconds
  , metricErrorsTotal
  , metricInFlightRequests
  , metricQueueDepth

    -- * Metric Aggregation
  , MetricsCollector
  , newMetricsCollector
  , incrementCounter
  , setGauge
  , observeHistogram
  , collectMetrics
  ) where

import Control.Concurrent.STM
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.List (sort)
import Data.Text (Text)
import Data.Word (Word64)
import Sentinel.Agent.Types
import UnliftIO (MonadIO, liftIO)

--------------------------------------------------------------------------------
-- Report Builders
--------------------------------------------------------------------------------

-- | Create a new metrics report
newMetricsReport ::
  -- | Agent ID
  Text ->
  -- | Timestamp (milliseconds)
  Word64 ->
  -- | Reporting interval (milliseconds)
  Word64 ->
  -- | Counter metrics
  [CounterMetric] ->
  -- | Gauge metrics
  [GaugeMetric] ->
  -- | Histogram metrics
  [HistogramMetric] ->
  MetricsReport
newMetricsReport agentId timestamp interval counters gauges histograms =
  MetricsReport
    { mrAgentId = agentId
    , mrTimestampMs = timestamp
    , mrIntervalMs = interval
    , mrCounters = counters
    , mrGauges = gauges
    , mrHistograms = histograms
    }

-- | Create an empty metrics report
emptyMetricsReport :: Text -> Word64 -> Word64 -> MetricsReport
emptyMetricsReport agentId timestamp interval =
  newMetricsReport agentId timestamp interval [] [] []

--------------------------------------------------------------------------------
-- Counter Metrics
--------------------------------------------------------------------------------

-- | Create a simple counter metric
counterMetric :: Text -> Word64 -> CounterMetric
counterMetric name value =
  CounterMetric
    { ctrName = name
    , ctrHelp = Nothing
    , ctrLabels = HM.empty
    , ctrValue = value
    }

-- | Create a counter metric with labels
counterWithLabels :: Text -> HashMap Text Text -> Word64 -> CounterMetric
counterWithLabels name labels value =
  CounterMetric
    { ctrName = name
    , ctrHelp = Nothing
    , ctrLabels = labels
    , ctrValue = value
    }

-- | Create a counter metric with help text
counterWithHelp :: Text -> Text -> Word64 -> CounterMetric
counterWithHelp name help value =
  CounterMetric
    { ctrName = name
    , ctrHelp = Just help
    , ctrLabels = HM.empty
    , ctrValue = value
    }

--------------------------------------------------------------------------------
-- Gauge Metrics
--------------------------------------------------------------------------------

-- | Create a simple gauge metric
gaugeMetric :: Text -> Double -> GaugeMetric
gaugeMetric name value =
  GaugeMetric
    { gaugeName = name
    , gaugeHelp = Nothing
    , gaugeLabels = HM.empty
    , gaugeValue = value
    }

-- | Create a gauge metric with labels
gaugeWithLabels :: Text -> HashMap Text Text -> Double -> GaugeMetric
gaugeWithLabels name labels value =
  GaugeMetric
    { gaugeName = name
    , gaugeHelp = Nothing
    , gaugeLabels = labels
    , gaugeValue = value
    }

-- | Create a gauge metric with help text
gaugeWithHelp :: Text -> Text -> Double -> GaugeMetric
gaugeWithHelp name help value =
  GaugeMetric
    { gaugeName = name
    , gaugeHelp = Just help
    , gaugeLabels = HM.empty
    , gaugeValue = value
    }

--------------------------------------------------------------------------------
-- Histogram Metrics
--------------------------------------------------------------------------------

-- | Create a histogram metric
histogramMetric ::
  -- | Metric name
  Text ->
  -- | Sum of observed values
  Double ->
  -- | Count of observations
  Word64 ->
  -- | Histogram buckets
  [HistogramBucket] ->
  HistogramMetric
histogramMetric name sumVal count buckets =
  HistogramMetric
    { histName = name
    , histHelp = Nothing
    , histLabels = HM.empty
    , histSum = sumVal
    , histCount = count
    , histBuckets = buckets
    }

-- | Create a histogram metric with labels
histogramWithLabels ::
  Text ->
  HashMap Text Text ->
  Double ->
  Word64 ->
  [HistogramBucket] ->
  HistogramMetric
histogramWithLabels name labels sumVal count buckets =
  HistogramMetric
    { histName = name
    , histHelp = Nothing
    , histLabels = labels
    , histSum = sumVal
    , histCount = count
    , histBuckets = buckets
    }

-- | Create a histogram bucket
histogramBucket ::
  -- | Upper bound (le)
  Double ->
  -- | Cumulative count
  Word64 ->
  HistogramBucket
histogramBucket = HistogramBucket

-- | Default latency buckets (in seconds)
--
-- @[0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0, +Inf]@
defaultLatencyBuckets :: [Double]
defaultLatencyBuckets =
  [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0, 1 / 0]

--------------------------------------------------------------------------------
-- Metrics Collector
--------------------------------------------------------------------------------

-- | Mutable metrics collector for tracking metrics
data MetricsCollector = MetricsCollector
  { mcCounters :: !(TVar (HashMap Text (TVar Word64)))
  , mcGauges :: !(TVar (HashMap Text (TVar Double)))
  , mcHistograms :: !(TVar (HashMap Text (TVar HistogramState)))
  }

-- | Internal state for histogram
data HistogramState = HistogramState
  { hsSum :: !Double
  , hsCount :: !Word64
  , hsBuckets :: ![Double]  -- Upper bounds
  , hsCounts :: ![Word64]   -- Cumulative counts per bucket
  }

-- | Create a new metrics collector
newMetricsCollector :: MonadIO m => m MetricsCollector
newMetricsCollector = liftIO $ do
  counters <- newTVarIO HM.empty
  gauges <- newTVarIO HM.empty
  histograms <- newTVarIO HM.empty
  pure MetricsCollector
    { mcCounters = counters
    , mcGauges = gauges
    , mcHistograms = histograms
    }

-- | Increment a counter by the given amount
incrementCounter :: MonadIO m => MetricsCollector -> Text -> Word64 -> m ()
incrementCounter mc name delta = liftIO $ atomically $ do
  counters <- readTVar (mcCounters mc)
  case HM.lookup name counters of
    Just counterVar -> modifyTVar' counterVar (+ delta)
    Nothing -> do
      counterVar <- newTVar delta
      modifyTVar' (mcCounters mc) (HM.insert name counterVar)

-- | Set a gauge to the given value
setGauge :: MonadIO m => MetricsCollector -> Text -> Double -> m ()
setGauge mc name value = liftIO $ atomically $ do
  gauges <- readTVar (mcGauges mc)
  case HM.lookup name gauges of
    Just gaugeVar -> writeTVar gaugeVar value
    Nothing -> do
      gaugeVar <- newTVar value
      modifyTVar' (mcGauges mc) (HM.insert name gaugeVar)

-- | Record an observation in a histogram
observeHistogram ::
  MonadIO m =>
  MetricsCollector ->
  -- | Metric name
  Text ->
  -- | Bucket boundaries (excluding +Inf)
  [Double] ->
  -- | Observed value
  Double ->
  m ()
observeHistogram mc name bucketBounds value = liftIO $ atomically $ do
  histograms <- readTVar (mcHistograms mc)
  let bounds = sort bucketBounds ++ [1 / 0]  -- Add +Inf
  case HM.lookup name histograms of
    Just histVar -> modifyTVar' histVar (updateHistogram bounds value)
    Nothing -> do
      let initialCounts = map (const 0) bounds
          initial = HistogramState 0 0 bounds initialCounts
      histVar <- newTVar (updateHistogram bounds value initial)
      modifyTVar' (mcHistograms mc) (HM.insert name histVar)
  where
    updateHistogram :: [Double] -> Double -> HistogramState -> HistogramState
    updateHistogram bounds val hs =
      hs
        { hsSum = hsSum hs + val
        , hsCount = hsCount hs + 1
        , hsCounts = zipWith updateBucket (hsBuckets hs) (hsCounts hs)
        }
      where
        updateBucket bound count
          | val <= bound = count + 1
          | otherwise = count

-- | Collect all metrics into a report
collectMetrics ::
  MonadIO m =>
  MetricsCollector ->
  -- | Agent ID
  Text ->
  -- | Timestamp (ms)
  Word64 ->
  -- | Interval (ms)
  Word64 ->
  m MetricsReport
collectMetrics mc agentId timestamp interval = liftIO $ atomically $ do
  -- Collect counters
  counterMap <- readTVar (mcCounters mc)
  counters <- mapM readCounter (HM.toList counterMap)

  -- Collect gauges
  gaugeMap <- readTVar (mcGauges mc)
  gauges <- mapM readGauge (HM.toList gaugeMap)

  -- Collect histograms
  histMap <- readTVar (mcHistograms mc)
  histograms <- mapM readHistogram (HM.toList histMap)

  pure $ newMetricsReport agentId timestamp interval counters gauges histograms
  where
    readCounter (name, var) = do
      value <- readTVar var
      pure $ counterMetric name value

    readGauge (name, var) = do
      value <- readTVar var
      pure $ gaugeMetric name value

    readHistogram (name, var) = do
      hs <- readTVar var
      let buckets = zipWith HistogramBucket (hsBuckets hs) (hsCounts hs)
      pure $ histogramMetric name (hsSum hs) (hsCount hs) buckets
