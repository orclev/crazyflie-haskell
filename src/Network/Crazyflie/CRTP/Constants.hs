module Network.Crazyflie.CRTP.Constants
    ( CRTPPort (..)
    , crtpPortToByteConstant
    , byteToCrtpPortConstant
    ) where

import Data.Word

data CRTPPort = Console | Param | Commander | Logging | Debugdriver
    | Linkctrl | All deriving (Show, Eq)

byteToCrtpPortConstant :: Word8 -> CRTPPort
byteToCrtpPortConstant port = case port of
    0x00 -> Console
    0x02 -> Param
    0x03 -> Commander
    0x05 -> Logging
    0x0e -> Debugdriver
    0x0f -> Linkctrl
    0xff -> All
    otherwise -> All

crtpPortToByteConstant :: CRTPPort -> Word8
crtpPortToByteConstant port = case port of
    Console -> 0x00
    Param -> 0x02
    Commander -> 0x03
    Logging -> 0x05
    Debugdriver -> 0x0e
    Linkctrl -> 0x0f
    All -> 0xff