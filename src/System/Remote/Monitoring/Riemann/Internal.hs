{-# LANGUAGE RecordWildCards
           , OverloadedStrings #-}
module System.Remote.Monitoring.Riemann.Internal where
import Prelude (Either(..), Bool(..), String, Float, Eq, Ord, Show, IO, undefined, (.), ($), fromIntegral)
import Control.Applicative
import Control.Exception
import Data.ByteString (ByteString)
import Data.Foldable hiding (sum)
import Data.Functor
import Data.HashMap.Lazy (HashMap, toList, lookup)
import qualified Data.HashMap.Lazy as Map
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.ProtocolBuffers (putField, getField, encodeMessage, decodeMessage)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Data.Text (Text, pack)
import Network
import Network.BSD (getHostName)
import System.Metrics
import System.Metrics.Distribution

import Riemann.Proto

data RiemannException
  = RiemannException Text
  | MissingOKField
  | DecodeException String
  deriving (Eq, Ord, Show)

instance Exception RiemannException

type StateFun = Value -> Text

data EventInfo
  = EventInfo
  { _service     :: Maybe Text
  , _description :: Maybe Text
  , _tags        :: [Text]
  , _ttl         :: Maybe Float
  , _toState     :: Maybe StateFun
  }

defaultEventInfo :: EventInfo
defaultEventInfo = EventInfo Nothing Nothing [] Nothing Nothing

type EventInfos = HashMap Text EventInfo

data RiemannConfig
  = RiemannConfig
  { _host       :: !Text
  , _eventInfos :: !EventInfos
  }

sampleToEvent :: RiemannConfig -> Text -> Value -> [Event]
sampleToEvent RiemannConfig{..} name s
  = let info      = fromMaybe defaultEventInfo $ Map.lookup name _eventInfos
        state     = ($ s) <$> _toState info
        sname     = fromMaybe name $ _service info
        evt       = defaultEvent { host = putField $ Just _host
                                 , state = putField state
                                 , service = putField $ Just sname
                                 , description = putField $ _description info
                                 , tags = putField $ _tags info
                                 , ttl = putField $ _ttl info
                                 }
        dist name = case Map.lookup name _eventInfos of
                      Nothing -> evt
                      Just  i -> evt { service = putField . Just $ fromMaybe name (_service i)
                                     , description = putField $ _description i <|> _description info
                                     , ttl = putField $ _ttl i <|> _ttl info
                                     , tags = putField $ _tags info <> _tags i
                                     }

    in case s of
         Counter      i -> pure $ evt { metric_sint64 = putField . Just $ fromIntegral i }
         Gauge        i -> pure $ evt { metric_sint64 = putField . Just $ fromIntegral i }
         Label        t -> pure $ evt { state = putField . Just $ fromMaybe t state }
         Distribution s -> [ (dist $ name <> ".mean") { metric_d = putField . Just $ mean s }
                           , (dist $ name <> ".variance") { metric_d = putField . Just $ variance s }
                           , (dist $ name <> ".count") { metric_sint64 = putField . Just . fromIntegral $ count s }
                           , (dist $ name <> ".sum") { metric_d = putField . Just $ sum s }
                           , (dist $ name <> ".min") { metric_d = putField . Just $ min s }
                           , (dist $ name <> ".max") { metric_d = putField . Just $ max s }
                           ]

storeToEvents :: RiemannConfig -> Store -> IO [Event]
storeToEvents c s = foldr step [] . Map.toList <$> sampleAll s
  where step (name, sample) r = sampleToEvent c name sample <> r

eventsToMsg :: [Event] -> Msg
eventsToMsg events = Msg mempty mempty mempty mempty (putField events)

msgToBytes :: Msg -> ByteString
msgToBytes = runPut . encodeMessage

readResponse :: ByteString -> ()
readResponse b = case runGet decodeMessage b of
                   Left err -> throw $ DecodeException err
                   Right m  -> case getField (ok m) of
                                 Nothing    -> throw MissingOKField
                                 Just True  -> ()
                                 Just False -> case getField (error m) of
                                                 Nothing  -> throw $ RiemannException "no error given"
                                                 Just err -> throw $ RiemannException err
