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
    , ifPacket
    , containsPacket
    , readPacket
    , maybeReadPacket
    , handleRetry
    , ifAck
    , throttleOnEmptyAck
    ) where

import ClassyPrelude hiding (when, unless)
import Network.Crazyflie
import Network.Crazyflie.CRTP.Types as X
import qualified Data.ByteString as BS
import Data.Binary
import Control.Wire
import qualified Data.List as L
import Data.Maybe (fromJust)

runSession :: (Show a, Eq a, Binary a) => Link -> CRTP (Maybe ACK) (CRTPPacket a) -> Crazyflie ()
runSession link wire = do
    setChannel $ linkChannel link
    setDataRate $ linkDataRate link
    setARC 10
    ack <- sendPacket emptyPacket'
    loop wire clockSession ack
    where
        emptyPacket' = toStrict $ encode (emptyPacket :: CRTPPacket ())
        loop w' session' ack = do
            (epacket, w, session) <- stepSession w' session' ack
            case epacket of
                Left ex -> do
                    liftIO $ putStrLn $ "Exception raised: " ++ show ex
                    ack <- sendPacket emptyPacket'
                    loop w session ack
                Right packet -> do
                    liftIO $ putStrLn $ "Sending packet " ++ show packet
                    ack <- sendPacket (toStrict $ encode packet)
                    loop w session ack

containsPacket :: CRTP ACK Bool
containsPacket = arr (isJust . ackData)

readPacket :: (Show a, Eq a, Binary a) => CRTP ACK (CRTPPacket a)
readPacket = arr ackData >>> unless (== Nothing) >>> arr (decode . fromStrict . fromJust)

maybeReadPacket :: (Show a, Eq a, Binary a) => CRTP ACK (Maybe (CRTPPacket a))
maybeReadPacket = ifW containsPacket (Just <$> readPacket) (pure Nothing)

sendEmptyPacket :: (Show b, Eq b, Binary b) => forall a. CRTP a (CRTPPacket b)
sendEmptyPacket = pure emptyPacket

ifPacket :: (Show a, Eq a, Binary a) => CRTP (CRTPPacket a) b -> CRTP ACK b
ifPacket w = readPacket >>> w

ifAck :: CRTP ACK a -> CRTP () a -> CRTP (Maybe ACK) a
ifAck a b = ifW (arr isJust) unwrapAck emptyCase
    where
        emptyCase = pure () >>> b
        unwrapAck = arr fromJust >>> a

handleRetry :: (Show a, Eq a, Binary a, Show b, Eq b, Binary b) => CRTP (CRTPPacket a) (CRTPPacket b)  -> CRTP (Maybe ACK) (CRTPPacket b)
handleRetry w = ifAck (throttleOnEmptyAck (ifPacket w)) sendEmptyPacket

throttleOnEmptyAck :: CRTP ACK a -> CRTP ACK a
throttleOnEmptyAck w = windowList 10 >>> ifW tenSamples continue checkWindow
    where
        continue = arr L.last >>> w
        tenSamples = arr ((< 10) . L.length)
        allEmpty = arr (L.all (isNothing . ackData))
        checkWindow = ifW allEmpty (after 0.01 >>> continue) continue

tapWire :: (Show a, Eq a) => CRTP a a
tapWire = proc packet -> do
        p' <- id -< packet
        perform -< liftIO . putStrLn $ "Tap: " ++ show p'
        returnA -< p'

onPort :: CRTPPort -> CRTP (CRTPPacket a) (CRTPPacket a)
onPort port = when isOnPort
    where
        isOnPort packet = (crtpPort packet) == port

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