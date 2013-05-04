module Network.Crazyflie.CRTP.Types
    ( module X
    , Link (..)
    , CRTP
    , CRTPPacket (..)
    , mkCRTPPacket
    , emptyPacket
    , packCRTPPacket
    ) where

import Network.Crazyflie.Types as X
import Network.Crazyflie.CRTP.Constants as X
import Control.Wire
import Control.Monad.Reader
import Data.ByteString as BS
import Data.Word
import Data.Bits ((.&.), (.|.), shiftR, shiftL)

-- For some reason you can't use Crzyflie type here, needs to be fully expanded
type CRTP a b = WireM (ReaderT CrazyflieState IO) a b

data Link = Link {
    linkRadioIndex :: Int,
    linkChannel :: Channel,
    linkDataRate :: DataRate
} deriving Eq

instance Show Link where
    show link = "radio://" ++ index ++ "/" ++ channel++ "/"  ++ rate
        where
            index = show $ linkRadioIndex link
            channel = show $ linkChannel link
            rate = case linkDataRate link of
                    DR_250KPS -> "250K"
                    DR_1MPS -> "1M"
                    DR_2MPS -> "2M"

data CRTPPacket = CRTPPacket {
    crtpPacketHeader :: !Word8,
    crtpPacketSize :: !Int,
    crtpPacketPort :: !CRTPPort,
    crtpPacketChannel :: !Word8,
    crtpPacketData :: Maybe ByteString
} deriving (Eq)

instance Show CRTPPacket where
    show packet = port ++ ": " ++ channel ++ " " ++ dat
        where
            port = show $ crtpPacketPort packet
            channel = show $ crtpPacketChannel packet
            dat = show $ crtpPacketData packet

emptyPacket :: CRTPPacket
emptyPacket = CRTPPacket 0xff 0 All 0 Nothing

mkCRTPPacket :: ACK -> CRTPPacket
mkCRTPPacket ack = CRTPPacket header size port channel mdata
    where
        header = ackHeader ack
        size = BS.length $ ackData ack
        port = byteToCrtpPortConstant $ shiftR (header .&. 0xF0) 4
        channel = (header .&. 0x03)
        mdata = if size == 0 then Nothing else Just $ ackData ack

packCRTPPacket :: CRTPPacket -> ByteString
packCRTPPacket packet = maybe headerByte packBody $ crtpPacketData packet
    where
        port = shiftL (crtpPortToByteConstant $ crtpPacketPort packet) 4
        channel = crtpPacketChannel packet
        header = channel .|. port
        headerByte = BS.pack [header]
        packBody dat = BS.append headerByte dat