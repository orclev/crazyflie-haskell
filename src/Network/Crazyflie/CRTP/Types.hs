module Network.Crazyflie.CRTP.Types
    ( module X
    , Link (..)
    , CRTP
    , CRTPPacket (..)
    , mkCRTPPacket
    , emptyPacket
    , SetPoint (..)
    , toXMode
    , pointToPacket
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

data CRTPPacket a where
    CRTPPacket :: (Binary a, Eq a) => !CRTPPort -> !PacketChannel -> Maybe a -> CRTPPacket a

deriving instance Eq a => Eq (CRTPPacket a)

instance (Show a) => P.Show (CRTPPacket a) where
    show (CRTPPacket port channel dat) = "(" ++ port' ++ ", " ++ channel' ++ "): " ++ dat'
        where
            port' = show port
            channel' = show channel
            dat' = show dat

instance (Binary a, Eq a) => Binary (CRTPPacket a) where
    put (CRTPPacket port channel dat) = maybe headerByte packBody dat
        where
            shiftL' = flip shiftL
            port' = shiftL' 4 . BS.head . toStrict . encode $ port
            header = channel .|. port'
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

mkCRTPPacket :: (Eq a, Binary a) => ACK -> CRTPPacket a
mkCRTPPacket = decode . encode

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