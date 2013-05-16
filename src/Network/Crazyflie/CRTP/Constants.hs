module Network.Crazyflie.CRTP.Constants
    ( CRTPPort (..)
    , LogChannel (..)
    , AllChannel (..)
    , ConsoleChannel (..)
    , ParamChannel (..)
    , CommanderChannel (..)
    , DebugdriverChannel (..)
    , LinkctrlChannel (..)
    ) where

import ClassyPrelude
import Data.Word
import Data.Binary
import Data.Bits

data LogChannel = LogTOC | LogSettings | LogData deriving (Show, Eq)
data AllChannel = AllChannel | AllBroadcast deriving (Show, Eq)
data ConsoleChannel = ConsoleChannel deriving (Show, Eq)
data ParamChannel = ParamChannel deriving (Show, Eq)
data CommanderChannel = CommanderChannel deriving (Show, Eq)
data DebugdriverChannel = DebugdriverChannel deriving (Show, Eq)
data LinkctrlChannel = LinkctrlChannel deriving (Show, Eq)
data CRTPPort = Console ConsoleChannel | Param ParamChannel | Commander CommanderChannel
    | Logging LogChannel | Debugdriver DebugdriverChannel
    | Linkctrl LinkctrlChannel | All AllChannel deriving (Show, Eq)
instance Binary CRTPPort where
    put (Console _) = putWord8 $ encodePortAndChannel 0x00 0x00
    put (Param _) = putWord8 $ encodePortAndChannel 0x02 0x00
    put (Commander _) = putWord8 $ encodePortAndChannel 0x03 0x00
    put (Logging c) = putWord8 $ encodePortAndChannel 0x05 chan
        where chan = case c of
                LogTOC -> 0x00
                LogSettings -> 0x01
                LogData -> 0x02
    put (Debugdriver _) = putWord8 $ encodePortAndChannel 0x0e 0x00
    put (Linkctrl _) = putWord8 $ encodePortAndChannel 0x0f 0x00
    put (All c) = putWord8 $ encodePortAndChannel 0xff chan
        where chan = case c of
                AllChannel -> 0x00
                AllBroadcast -> 0xff
    get = do
        p <- getWord8
        case shiftR (p .&. 0xF0) 4 of
            0x00 -> return (Console ConsoleChannel)
            0x02 -> return (Param ParamChannel)
            0x03 -> return (Commander CommanderChannel)
            0x05 -> case (p .&. 0x03) of
                0x00 -> return (Logging LogTOC)
                0x01 -> return (Logging LogSettings)
                0x02 -> return (Logging LogData)
            0x0e -> return (Debugdriver DebugdriverChannel)
            0x0f -> return (Linkctrl LinkctrlChannel)
            0xff -> case (p .&. 0x03) of
                0x00 -> return (All AllChannel)
                0xff -> return (All AllBroadcast)
            otherwise -> return (All AllChannel)

encodePortAndChannel :: Word8 -> Word8 -> Word8
encodePortAndChannel port channel = portBits .|. channelBits
    where
        portBits = (shiftL port 4) .&. 0xF0
        channelBits = channel .&. 0x03
