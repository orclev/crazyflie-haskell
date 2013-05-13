module Network.Crazyflie.CRTP.Types
    ( module X
    , Link (..)
    , CRTP
    , CRTPPacket (..)
    , ackToCRTPPacket
    , emptyPacket
    , SetPoint (..)
    , toXMode
    , pointToPacket
    , crtpPort
    , crtpChannel
    , crtpData
    ) where

import ClassyPrelude
import qualified Prelude as P
import qualified Data.ByteString as BS
import Network.Crazyflie.Types as X
import Network.Crazyflie.CRTP.Constants as X
import Control.Wire
import Control.Monad.Reader
import Data.Word
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.IEEE754

-- For some reason you can't use Crzyflie type here, needs to be fully expanded
type CRTP a b = WireM (ReaderT CrazyflieState IO) a b

data Link = Link {
    linkRadioIndex :: Int,
    linkChannel :: Channel,
    linkDataRate :: DataRate
} deriving Eq

instance P.Show Link where
    show link = "radio://" ++ index ++ "/" ++ channel++ "/"  ++ rate
        where
            index = show $ linkRadioIndex link
            channel = show $ linkChannel link
            rate = case linkDataRate link of
                    DR_250KPS -> "250K"
                    DR_1MPS -> "1M"
                    DR_2MPS -> "2M"

type PacketChannel = Word8
type RetryCount = Int

data CRTPPacket a where
    CRTPPacket :: (Binary a, Eq a) => !CRTPPort -> !PacketChannel -> Maybe a -> CRTPPacket a

crtpPort :: CRTPPacket a -> CRTPPort
crtpPort (CRTPPacket port _ _) = port
crtpChannel :: CRTPPacket a -> PacketChannel
crtpChannel (CRTPPacket _ channel _) = channel
crtpData :: CRTPPacket a -> Maybe a
crtpData (CRTPPacket _ _ dat) = dat

deriving instance Eq a => Eq (CRTPPacket a)

instance (Show a) => P.Show (CRTPPacket a) where
    show packet = "(" ++ port ++ ", " ++ channel ++ "): " ++ dat
        where
            port = show $ crtpPort packet
            channel = show $ crtpChannel packet
            dat = show $ crtpData packet

instance (Binary a, Eq a) => Binary (CRTPPacket a) where
    put packet = maybe headerByte packBody (crtpData packet)
        where
            shiftL' = flip shiftL
            port = shiftL' 4 . BS.head . toStrict . encode $ crtpPort packet
            header = (crtpChannel packet) .|. port
            headerByte = putWord8 header
            packBody dat = do
                headerByte
                put dat
    get = do
        header <- getWord8
        let port = decode $ pack $ [shiftR (header .&. 0xF0) 4]
            channel = decode $ pack $ [header .&. 0x03]
        x <- remaining
        if x == 0 then
            return $ CRTPPacket port channel Nothing
            else
                do
                    dat <- get
                    return $ CRTPPacket port channel (Just dat)

emptyPacket :: (Eq a, Binary a) => CRTPPacket a
emptyPacket = CRTPPacket All 0xff Nothing

ackToCRTPPacket :: (Eq a, Binary a) => ACK -> Maybe (CRTPPacket a)
ackToCRTPPacket ack = (decode . fromStrict) <$> ackData ack

data SetPoint = SetPoint {
    pointRoll :: Float,
    pointPitch :: Float,
    pointYaw :: Float,
    pointThrust :: Word16
} deriving (Show, Eq)

toXMode :: SetPoint -> SetPoint
toXMode point = point {
    pointRoll = 0.707 * (pointRoll point - pointPitch point),
    pointPitch = 0.707 * (pointRoll point + pointPitch point)
}

instance Binary SetPoint where
    put point = do
        putFloat32le $ pointRoll point
        putFloat32le $ pointPitch point * (-1)
        putFloat32le $ pointYaw point
        putWord16le $ pointThrust point
    get = SetPoint <$> getFloat32le <*> getFloat32le <*> getFloat32le <*> getWord16le

pointToPacket :: SetPoint -> CRTPPacket SetPoint
pointToPacket point = CRTPPacket Commander 0 (Just point)