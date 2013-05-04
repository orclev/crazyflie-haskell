module Network.Crazyflie.Types
    ( module X
    , Crazyflie
    , CrazyflieState (..)
    , ACK (..)
    , CrazyRadio
    , DataRate (..)
    , Channel
    , ARC
    , ARDBytes
    , ARDTime
    , RadioPower (..)
    , Start
    , Stop
    , Packet
    , RadioAddress (..)
    , CrazyflieException (..)
    , mkRadioAddress
    , coerceInt
    )where

import Prelude
import qualified Prelude as P
import Control.Monad.IO.Class as X -- export liftIO
import Control.Exception
import Data.ByteString as BS
import System.USB
import Control.Monad.Reader
import Data.Typeable
import Data.Word
import Data.Bits (Bits, shiftR)

type CrazyRadio = Device
data DataRate = DR_250KPS | DR_1MPS | DR_2MPS deriving (Eq)
type Channel = Value
type ARC = Value
type ARDBytes = Word8
type ARDTime = Word8
data RadioPower = P_M18DBM | P_M12DBM | P_M6DBM | P_0DBM deriving (Eq)
type Start = Value
type Stop = Index
type Packet = ByteString

data CrazyflieState = CFS {
    radioHandle :: DeviceHandle,
    bulkInputAddress :: EndpointAddress,
    bulkOutputAddress :: EndpointAddress
}  deriving (Eq)
type Crazyflie a = ReaderT CrazyflieState IO a
newtype RadioAddress = RadioAddress ByteString
    deriving (Eq)
data CrazyflieException = InvalidAddressException
    deriving (Show, Typeable)
instance Exception CrazyflieException

instance Show RadioAddress where
    show (RadioAddress a) = show $ BS.unpack a

data ACK = ACK {
    ackAck :: Bool,
    ackPowerDet :: Bool,
    ackRetry :: Int,
    ackData :: ByteString
}

minAddress :: Num a => a
minAddress = 0x0100000000
maxAddress :: Num a => a
maxAddress = 0xFFFFFFFFFF

mkRadioAddress :: Integer -> RadioAddress
mkRadioAddress a = if a < minAddress || a > maxAddress then throw InvalidAddressException else RadioAddress ab
    where
        ab = pack $ chunkNum a

chunkNum :: (Bits a, Integral a) => a -> [Word8]
chunkNum = P.reverse . chunkNum'
    where
        chunkNum' a = if a < 256 then [coerceInt a] else (coerceInt (m a)) : chunkNum' (shiftR a 8)
        m x = x `mod` 256

coerceInt :: (Integral a, Integral b) => a -> b
coerceInt = fromInteger . toInteger