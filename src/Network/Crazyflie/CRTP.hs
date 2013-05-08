module Network.Crazyflie.CRTP
    ( module X
    , getChannelList
    , findCrazyRadios
    , findFirstCrazyRadio
    , withCrazyRadio
    , runSession
    , sendEmptyPacket
    , tapWire
    , onPort
    ) where

import ClassyPrelude hiding (when)
import Network.Crazyflie
import Network.Crazyflie.CRTP.Types as X
import qualified Data.ByteString as BS
import Data.Binary
import Control.Wire

runSession :: Link -> CRTP CRTPPacket CRTPPacket -> Crazyflie ()
runSession link wire = do
    setChannel $ linkChannel link
    setDataRate $ linkDataRate link
    setARC 10
    loop wire clockSession emptyPacket
    where
        loop w' session' packet' = do
            mack <- sendPacket (toStrict $ encode packet')
            let ack = maybe emptyPacket mkCRTPPacket mack
            (mpacket, w, session) <- stepSession w' session' ack
            case mpacket of
                Left ex -> loop w session emptyPacket
                Right packet -> loop w session packet

sendEmptyPacket :: forall a. CRTP a CRTPPacket
sendEmptyPacket = pure emptyPacket

tapWire :: CRTP CRTPPacket CRTPPacket
tapWire = proc packet -> do
        p' <- changed -< packet
        perform -< liftIO . putStrLn $ show p'
        returnA -< p'

onPort :: CRTPPort -> CRTP CRTPPacket CRTPPacket
onPort port = when isOnPort
    where
        isOnPort packet = crtpPacketPort packet == port

getChannelList :: Crazyflie [Link]
getChannelList = do
    setARC 1
    links <- mapM scanRate [DR_250KPS, DR_1MPS, DR_2MPS]
    return $ concat links
    where
        scanRate dr = do
            setDataRate dr
            channels <- scanChannels 0 125 packet
            return $ map (\x -> Link 0 (coerceInt x) dr) (BS.unpack channels)
        packet = BS.pack [0xff]