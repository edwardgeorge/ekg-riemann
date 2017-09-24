module System.Remote.Monitoring.Riemann
  ( module System.Remote.Monitoring.Riemann
  , StateFun
  , EventInfo(..)
  , defaultEventInfo
  , EventInfos
  , RiemannConfig(..)
  ) where
import Prelude (IO, undefined)
import Control.Concurrent
import Network.Socket
import System.Metrics (Store)

import System.Remote.Monitoring.Riemann.Internal

defaultRiemannConfig :: IO RiemannConfig
defaultRiemannConfig = (\hn -> RiemannConfig (pack hn) mempty) <$> getHostName

-- lenses for internal types

service :: Functor f => (Maybe Text -> f (Maybe Text)) -> EventInfo -> f EventInfo
service f s = (\v -> s { _service = v }) <$> f (_service s)

description :: Functor f => (Maybe Text -> f (Maybe Text)) -> EventInfo -> f EventInfo
description f s = (\v -> s { _description = v }) <$> f (_description s)

tags :: Functor f => ([Text] -> f [Text]) -> EventInfo -> f EventInfo
tags f s = (\v -> s { _tags = v }) <$> f (_tags s)

ttl :: Functor f => (Maybe Float -> f (Maybe Float)) -> EventInfo -> f EventInfo
ttl f s = (\v -> s { _ttl = v }) <$> f (_ttl s)

toState :: Functor f => (Maybe StateFun -> f (Maybe StateFun)) -> EventInfo -> f EventInfo
toState f s = (\v -> s { _toState = v }) <$> f (_toState s)


host :: Functor f => (Text -> f Text) -> RiemannConfig -> f RiemannConfig
host f s = (\v -> s { _host = v }) <$> f (_host s)

eventInfos :: Functor f => (EventInfos -> f EventInfos) -> RiemannConfig -> f RiemannConfig
eventInfos f s = (\v -> s { _eventInfos = v }) <$> f (_eventInfos s)

--

connectRiemann :: RiemannConfig -> IO Socket
connectRiemann = undefined

sendRiemann :: Socket -> Store -> IO ()
sendRiemann = undefined

forkRiemann :: RiemannConfig -> Store -> IO ThreadId
forkRiemann = undefined
