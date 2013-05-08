module Network.Crazyflie.CRTP.Types
    ( module X
    , Link (..)
    , CRTP
    , CRTPPacket (..)
    , mkCRTPPacket
    , emptyPacket
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

data CRTPPacket = CRTPPacket {
    crtpPacketHeader :: !Word8,
    crtpPacketSize :: !Int,
    crtpPacketPort :: !CRTPPort,
    crtpPacketChannel :: !Word8,
    crtpPacketData :: Maybe ByteString
} deriving (Eq)

instance P.Show CRTPPacket where
    show packet = port ++ ": " ++ channel ++ " " ++ dat
        where
            port = show $ crtpPacketPort packet
            channel = show $ crtpPacketChannel packet
            dat = show $ crtpPacketData packet

instance Binary CRTPPacket where
    put packet = maybe headerByte packBody $ crtpPacketData packet
        where
            shiftL' = flip shiftL
            port = shiftL' 4 . BS.head . toStrict . encode $ crtpPacketPort packet
            channel = crtpPacketChannel packet
            header = channel .|. port
            headerByte = putWord8 header
            packBody dat = do
                headerByte
                putByteString dat
    get = do
        header <- getWord8
        let port = decode $ pack $ [shiftR (header .&. 0xF0) 4]
            channel = decode $ pack $ [header .&. 0x03]
        x <- remaining
        if x == 0 then
                return $ CRTPPacket header 0 port channel Nothing
            else
                do
                    dat <- getByteString $ coerceInt x
                    return $ CRTPPacket header (BS.length dat) port channel (Just dat)

emptyPacket :: CRTPPacket
emptyPacket = CRTPPacket 0xff 0 All 0 Nothing

mkCRTPPacket :: ACK -> CRTPPacket
mkCRTPPacket = decode . encode