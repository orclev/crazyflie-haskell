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
    , RadioAddress ()
    , CrazyflieException (..)
    , mkRadioAddress
    , coerceInt
    )where

import ClassyPrelude
import qualified Prelude as P
import Network.Crazyflie.Util
import Control.Monad.IO.Class as X -- export liftIO
import Control.Exception
import System.USB
import Control.Monad.Reader
import Data.Typeable
import Data.Word
import Data.Bits (Bits, shiftR, shiftL, (.&.), (.|.))
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

type CrazyRadio = Device
type Channel = Value
type ARC = Value
type ARDBytes = Word8
type ARDTime = Word8
type Start = Value
type Stop = Index
type Packet = ByteString
type Crazyflie a = ReaderT CrazyflieState IO a

data RadioPower = P_M18DBM | P_M12DBM | P_M6DBM | P_0DBM deriving (Eq)
instance Binary RadioPower where
    put P_M18DBM = putWord8 0
    put P_M12DBM = putWord8 1
    put P_M6DBM = putWord8 2
    put P_0DBM = putWord8 3
    get = do
        p <- getWord8
        case p of
            0 -> return P_M18DBM
            1 -> return P_M12DBM
            2 -> return P_M6DBM
            3 -> return P_0DBM

data DataRate = DR_250KPS | DR_1MPS | DR_2MPS deriving (Eq)
instance Binary DataRate where
    put DR_250KPS = putWord8 0
    put DR_1MPS = putWord8 1
    put DR_2MPS = putWord8 2
    get = do
        dr <- getWord8
        case dr of
            0 -> return DR_250KPS
            1 -> return DR_1MPS
            2 -> return DR_2MPS

data CrazyflieState = CFS {
    radioHandle :: DeviceHandle,
    bulkInputAddress :: EndpointAddress,
    bulkOutputAddress :: EndpointAddress
}  deriving (Eq)

newtype RadioAddress = RadioAddress Integer
    deriving (Eq)

minAddress :: Num a => a
minAddress = 0x0100000000
maxAddress :: Num a => a
maxAddress = 0xFFFFFFFFFF

instance Bounded RadioAddress where
    minBound = RadioAddress minAddress
    maxBound = RadioAddress maxAddress

instance Enum RadioAddress where
    succ (RadioAddress x) = if x < maxAddress then RadioAddress $! x+1 else error "RadioAddress: tried to take `succ' of maxBound"
    pred (RadioAddress x) = if x > minAddress then RadioAddress $! x-1 else error "RadioAddress: tried to take `pred' of minBound"
    toEnum x = if x >= 0 && ox < maxAddress then RadioAddress (coerceInt ox) else error "RadioAddress: toEnum called with invalid bound"
        where
            ox = x + minAddress
    fromEnum (RadioAddress x) = coerceInt $ x - minAddress

-- Need to do crazy stuff here because some idiot decided to make the address 5 bytes long
instance Binary RadioAddress where
    put (RadioAddress x) = putByteString . BS.pack . drop 3 . BS.unpack $ toStrict x'
        where
            x' = runPut . putWord64be $ coerceInt x
    get = do
        x <- getBytes 5
        let x' = runGet getWord64be  $ (pack [0,0,0]) `append` (fromStrict x)
        if x' < minBound || x' > maxBound then throw InvalidAddressException else
            return $ RadioAddress $ coerceInt x'

instance P.Show RadioAddress where
    show (RadioAddress a) = P.show a

data CrazyflieException = InvalidAddressException
    deriving (Show, Typeable)

instance Exception CrazyflieException

data ACK = ACK {
    ackAck :: !Bool,
    ackPowerDet :: !Bool,
    ackRetry :: !Int,
    ackHeader :: !Word8,
    ackData :: !ByteString
}

instance Binary ACK where
    put ack = do
        putWord8 byte0
        putByteString (ackData ack)
        where
            byte0 = ackBit .|. powerBit .|. retryBits
            ackBit = if ackAck ack then 0x01 else 0x00
            powerBit = if ackPowerDet ack then 0x02 else 0x00
            retryBits = shiftL (coerceInt $ ackRetry ack) 4
    get = do
        header <- getWord8
        let ack = header .&. 0x01 /= 0
            powerDet = header .&. 0x02 /= 0
            retry = coerceInt $ shiftR header 4
        r <- remaining
        dat <- getByteString $ coerceInt r
        return $ ACK ack powerDet retry header dat

mkRadioAddress :: Integer -> RadioAddress
mkRadioAddress a = if a < minAddress || a > maxAddress then throw InvalidAddressException else RadioAddress a