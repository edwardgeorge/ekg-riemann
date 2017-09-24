{-# LANGUAGE
    DataKinds
  , DeriveGeneric
#-}
module Riemann.Proto where
import Prelude (Float, Bool, Double, Show)
import Data.Int
import Data.Monoid
import Data.ProtocolBuffers
import Data.Text
import GHC.Generics (Generic)

{-
// Deprecated; state was used by early versions of the protocol, but not any
// more.
message State {
  optional int64 time = 1;
  optional string state = 2;
  optional string service = 3;
  optional string host = 4;
  optional string description = 5;
  optional bool once = 6;
  repeated string tags = 7;
  optional float ttl = 8;
}
-}

data State
  = State
  { stTime        :: Optional 1 (Value Int64)
  , stState       :: Optional 2 (Value Text)
  , stService     :: Optional 3 (Value Text)
  , stHost        :: Optional 4 (Value Text)
  , stDescription :: Optional 5 (Value Text)
  , stOnce        :: Optional 6 (Value Bool)
  , stTags        :: Repeated 7 (Value Text)
  , stTtl         :: Optional 8 (Value Float)
  } deriving (Show, Generic)

instance Encode State
instance Decode State

{-
message Event {
  optional int64 time = 1;
  optional string state = 2;
  optional string service = 3;
  optional string host = 4;
  optional string description = 5;
  repeated string tags = 7;
  optional float ttl = 8;
  repeated Attribute attributes = 9;

  optional int64 time_micros = 10;
  optional sint64 metric_sint64 = 13;
  optional double metric_d = 14;
  optional float metric_f = 15;
}
-}

data Event
  = Event
  { time          :: !(Optional 1 (Value Int64))
  , state         :: !(Optional 2 (Value Text))
  , service       :: !(Optional 3 (Value Text))
  , host          :: !(Optional 4 (Value Text))
  , description   :: !(Optional 5 (Value Text))
  , tags          :: !(Repeated 7 (Value Text))
  , ttl           :: !(Optional 8 (Value Float))
  , attributes    :: !(Repeated 9 (Message Attribute))

  , time_micros   :: !(Optional 10 (Value Int64))
  , metric_sint64 :: !(Optional 13 (Value (Signed Int64)))
  , metric_d      :: !(Optional 14 (Value Double))
  , metric_f      :: !(Optional 15 (Value Float))
  } deriving (Show, Generic)

instance Encode Event
instance Decode Event

defaultEvent :: Event
defaultEvent = Event
               { time          = mempty
               , state         = mempty
               , service       = mempty
               , host          = mempty
               , description   = mempty
               , tags          = mempty
               , ttl           = mempty
               , attributes    = mempty
               , time_micros   = mempty
               , metric_sint64 = mempty
               , metric_d      = mempty
               , metric_f      = mempty
               }

{-
message Query {
  optional string string = 1;
}
-}
data Query
  = Query
  { string :: Optional 1 (Value Text)
  } deriving (Show, Generic)

instance Encode Query
instance Decode Query

{-
message Msg {
  optional bool ok = 2;
  optional string error = 3;
  repeated State states = 4;
  optional Query query = 5;
  repeated Event events = 6;
}
-}

data Msg
  = Msg
  { ok     :: Optional 2 (Value Bool)
  , error  :: Optional 3 (Value Text)
  , states :: Repeated 4 (Message State)
  , query  :: Optional 5 (Message Query)
  , events :: Repeated 6 (Message Event)
  } deriving (Show, Generic)

instance Encode Msg
instance Decode Msg

{-
message Attribute {
  required string key = 1;
  optional string value = 2;
}
-}
data Attribute
  = Attribute
  { key   :: Required 1 (Value Text)
  , value :: Optional 2 (Value Text)
  } deriving (Show, Generic)

instance Encode Attribute
instance Decode Attribute
