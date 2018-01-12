module Network.Statsd (
  statsdClient,
  fromURI,

  Stat,
  Type(..),
  fmtDatagram,

  increment,
  decrement,
  count,
  gauge,
  timing,
  histogram,
) where

import Network.Statsd.UdpClient(UdpClient, fromURI, send)

import Control.Monad
import Data.Maybe
import Data.Time.Units
import Text.Printf
import Network.URI

type Stat = String
data Type = Count | Gauge | Timing | Histogram
instance Show Type where
  show Count = "c"
  show Gauge = "g"
  show Timing = "ms"
  show Histogram = "h"


class (Show a) => Datagram a where
   fmtDatagram :: Stat -> a -> Type -> String
   fmtDatagram stat value statType = printf "%s:%s|%s" stat (show value) (show statType)

instance Datagram Integer
instance Datagram Double


statsdClient :: String -> IO UdpClient
statsdClient = fromURI . fromJust . parseURI

increment :: UdpClient -> Stat -> IO ()
increment client stat = count client stat 1

decrement :: UdpClient -> Stat -> IO ()
decrement client stat = count client stat (-1)

count :: UdpClient -> Stat -> Integer -> IO ()
count client stat value = void . send client $ fmtDatagram stat value Count

class (Datagram a) => Gauge a where
  gauge :: UdpClient -> Stat -> a -> IO ()
  gauge client stat value = void . send client $ fmtDatagram stat value Gauge

instance Gauge Integer
instance Gauge Double


timing :: UdpClient -> Stat -> Millisecond -> IO ()
timing client stat value = void . send client $ fmtDatagram stat ((fromIntegral value)::Integer)  Timing


class (Datagram a) => Histogram a where
  histogram :: UdpClient -> Stat -> a -> IO ()
  histogram client stat value = void . send client $ fmtDatagram stat value Histogram

instance Histogram Double
instance Histogram Integer
