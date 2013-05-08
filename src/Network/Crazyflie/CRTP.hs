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

runSession :: (Show a, Eq a, Binary a, Show b, Eq b, Binary b) => Link -> CRTP (CRTPPacket a) (CRTPPacket b) -> Crazyflie ()
runSession link wire = do
    setChannel $ linkChannel link
    setDataRate $ linkDataRate link
    setARC 10
    loop wire clockSession emptyPacket
    where
        loop w' session' packet' = do
            liftIO $ putStrLn $ "Sending packet " ++ show packet'
            mack <- sendPacket (toStrict $ encode packet')
            let ack = maybe emptyPacket mkCRTPPacket mack
            (mpacket, w, session) <- stepSession w' session' ack
            case mpacket of
                Left ex -> loop w session emptyPacket
                Right packet -> loop w session packet

sendEmptyPacket :: (Show b, Eq b, Binary b) => forall a. CRTP a (CRTPPacket b)
sendEmptyPacket = pure emptyPacket

tapWire :: (Show a, Eq a) => CRTP (CRTPPacket a) (CRTPPacket a)
tapWire = proc packet -> do
        p' <- changed -< packet
        perform -< liftIO . putStrLn $ show p'
        returnA -< p'

onPort :: CRTPPort -> CRTP (CRTPPacket a) (CRTPPacket a)
onPort port = when isOnPort
    where
        isOnPort (CRTPPacket port' _ _) = port' == port

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