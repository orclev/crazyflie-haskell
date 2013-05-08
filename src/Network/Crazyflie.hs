module Network.Crazyflie
    ( module X
    , findCrazyRadios
    , findFirstCrazyRadio
    , withCrazyRadio
    , setDataRate
    , setPower
    , setARC
    , setARDBytes
    , setARDTime
    , setChannel
    , setAddress
    , setContCarrier
    , scanChannels
    , sendPacket
    ) where

import ClassyPrelude
import qualified Network.Crazyflie.Types as X
import Network.Crazyflie.Constants
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Control.Applicative
import Control.Monad.Reader
import Data.Typeable
import Data.Word
import Data.Bits ((.|.))
import Data.Binary

findCrazyRadios :: IO (Vector CrazyRadio)
findCrazyRadios = do
    ctx <- newCtx
    devices <- getDevices ctx
    V.filterM checkDevice devices
    where
        checkDevice :: Device -> IO Bool
        checkDevice device = isCrazyRadio <$> getDeviceDesc device

isCrazyRadio ::  DeviceDesc -> Bool
isCrazyRadio device = deviceVendorId device == cradioVid && deviceProductId device == cradioPid

findFirstCrazyRadio :: IO (Maybe CrazyRadio)
findFirstCrazyRadio = do
    ctx <- newCtx
    devices <- getDevices ctx
    descriptions <- V.mapM (\d -> (d,) <$> getDeviceDesc d) devices
    return $ fst <$> V.find (isCrazyRadio . snd) descriptions

isBulkInput ::  EndpointDesc -> Bool
isBulkInput endpoint = isBulk && isInput
    where
        isInput = isInAddress $ endpointAddress endpoint
        isBulk = endpointAttribs endpoint == Bulk

isBulkOutput :: EndpointDesc -> Bool
isBulkOutput endpoint = isBulk && isOutput
    where
        isOutput = isOutAddress $ endpointAddress endpoint
        isBulk = endpointAttribs endpoint == Bulk

isInAddress :: EndpointAddress -> Bool
isInAddress address = transferDirection address == In

isOutAddress :: EndpointAddress -> Bool
isOutAddress address = transferDirection address == Out

concatV :: Vector (Vector a) -> Vector a
concatV = V.foldl' (V.++) V.empty

findBulkInput :: Device -> IO (Maybe EndpointAddress)
findBulkInput device = do
    configDesc <- getConfigDesc device 0
    return $ endpointAddress <$> (V.find isBulkInput $ endpoints configDesc)
    where
        endpoints :: ConfigDesc -> Vector EndpointDesc
        endpoints = interfaceEndpoints . V.head . concatV . configInterfaces

findBulkOutput :: Device -> IO (Maybe EndpointAddress)
findBulkOutput device = do
    configDesc <- getConfigDesc device 0
    return $ endpointAddress <$> (V.find isBulkOutput $ endpoints configDesc)
    where
        endpoints :: ConfigDesc -> Vector EndpointDesc
        endpoints = interfaceEndpoints . V.head . concatV . configInterfaces


withCrazyRadio :: CrazyRadio -> (Crazyflie a) -> IO a
withCrazyRadio radio f = withDeviceHandle radio withHandle
    where
        withHandle dh = do
            (Just bi) <- findBulkInput radio
            (Just bo) <- findBulkOutput radio
            runReaderT (initRadio f) $ CFS dh bi bo
        initRadio :: (Crazyflie a) -> Crazyflie a
        initRadio f = do
            state <- ask
            let dh = radioHandle state
            liftIO $ setConfig dh (Just 1)
            liftIO $ claimInterface dh 0
            setDataRate DR_2MPS
            setChannel 2
            setContCarrier False
            setAddress (mkRadioAddress 0xE7E7E7E7E7)
            setPower P_0DBM
            setARC 3
            setARDBytes 32
            f

setDataRate :: DataRate -> Crazyflie ()
setDataRate dr = sendVendorSetup dataRateRequest drv 0
    where
        drv = coerceInt . BS.head . toStrict $ encode dr

setPower :: RadioPower -> Crazyflie ()
setPower p = sendVendorSetup radioPowerRequest pv 0
    where
        pv = coerceInt . BS.head . toStrict $ encode p

setARC :: ARC -> Crazyflie ()
setARC arc = sendVendorSetup radioARCRequest arc 0

setARDBytes :: ARDBytes -> Crazyflie ()
setARDBytes bytes = sendVendorSetup radioARDRequest (coerceInt $ 0x80 .|. bytes) 0

setARDTime :: ARDTime -> Crazyflie ()
setARDTime time = sendVendorSetup radioARDRequest (roundARDTime time) 0

roundARDTime :: ARDTime -> Value
roundARDTime t = if t' < 0 then 0 else if t' > 0xf then 0xf else coerceInt t'
    where
        t' = t `div` 250 - 1

setChannel :: Channel -> Crazyflie ()
setChannel ch = sendVendorSetup radioChannelRequest ch 0

setAddress :: RadioAddress -> Crazyflie ()
setAddress address = sendVendorSetupExact radioAddressRequest 0 0 $ toStrict $ encode address

setContCarrier :: Bool -> Crazyflie ()
setContCarrier True =  sendVendorSetup contCarrierRequest 1 0
setContCarrier False =  sendVendorSetup contCarrierRequest 0 0

scanChannels :: Start -> Stop -> Packet -> Crazyflie ByteString
scanChannels start stop packet = do
    sendVendorSetupExact scanChannelsRequest start stop packet
    getVendorSetup scanChannelsRequest 0 0 64

sendPacket :: Packet -> Crazyflie (Maybe ACK)
sendPacket packet = do
    state <- ask
    let dh = radioHandle state
    let inAddress = bulkInputAddress state
    let outAddress = bulkOutputAddress state
    liftIO $ handle ignoreException $ do
        writeBulk dh inAddress packet 500
        (response, _) <- readBulk dh outAddress 64 500
        return $ Just (decode $ fromStrict response)
    where
        ignoreException :: USBException -> IO (Maybe ACK)
        ignoreException _ = return Nothing

sendVendorSetup :: Request -> Value -> Index -> Crazyflie ()
sendVendorSetup request value index = do
    state <- ask
    let dh = radioHandle state
    liftIO $ control dh Vendor ToDevice request value index 1000

sendVendorSetupExact :: Request -> Value -> Index -> ByteString -> Crazyflie ()
sendVendorSetupExact request value index bs = do
    state <- ask
    let dh = radioHandle state
    liftIO $ writeControlExact dh Vendor ToDevice request value index bs 1000

getVendorSetup :: Request -> Value -> Index -> Size -> Crazyflie ByteString
getVendorSetup request value index size = do
    state <- ask
    let dh = radioHandle state
    fst <$> (liftIO $ readControl dh Vendor ToDevice request value index size 1000)

getVendorSetupExact :: Request -> Value -> Index -> Size -> Crazyflie ByteString
getVendorSetupExact request value index size = do
    state <- ask
    let dh = radioHandle state
    liftIO $ readControlExact dh Vendor ToDevice request value index size 1000