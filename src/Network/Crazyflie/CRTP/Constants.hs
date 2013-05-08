module Network.Crazyflie.CRTP.Constants
    ( CRTPPort (..)
    ) where

import ClassyPrelude
import Data.Word
import Data.Binary

data CRTPPort = Console | Param | Commander | Logging | Debugdriver
    | Linkctrl | All deriving (Show, Eq)
instance Binary CRTPPort where
    put Console = putWord8 0x00
    put Param = putWord8 0x02
    put Commander = putWord8 0x03
    put Logging = putWord8 0x05
    put Debugdriver = putWord8 0x0e
    put Linkctrl = putWord8 0x0f
    put All = putWord8 0xff
    get = do
        p <- getWord8
        case p of
            0x00 -> return Console
            0x02 -> return Param
            0x03 -> return Commander
            0x05 -> return Logging
            0x0e -> return Debugdriver
            0x0f -> return Linkctrl
            0xff -> return All
            otherwise -> return All