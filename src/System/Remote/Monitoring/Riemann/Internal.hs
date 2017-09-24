{-# LANGUAGE RecordWildCards #-}
module System.Remote.Monitoring.Riemann.Internal where
import Data.HashMap.Lazy (toList)
import Data.Text (Text)
import System.Metrics

import Riemann.Proto

data RiemannConfig = RiemannConfig { host :: Text }
  deriving (Eq, Ord, Show)

sampleToEvent :: RiemannConfig -> Text -> Value -> [Event]
sampleToEvent RiemannConfig{..} name s = case s of
  Counter      i -> undefined
  Gauge        i -> undefined
  Label        t -> undefined
  Distribution s -> undefined

storeToEvents :: RiemannConfig -> Store -> IO [Event]
storeToEvents c s = foldr step [] . toList <$> sampleAll s
  where step (name, sample) r = sampleToEvent c name sample ++ r
