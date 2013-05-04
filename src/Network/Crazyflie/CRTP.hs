module Network.Crazyflie.CRTP
    ( module X
    , getChannelList
    , findCrazyRadios
    , findFirstCrazyRadio
    , withCrazyRadio
    ) where

import Network.Crazyflie
import Network.Crazyflie.CRTP.Types as X
import qualified Data.ByteString as BS
import Control.Concurrent (forkIO, ThreadId, getNumCapabilities)
import Control.Concurrent.STM
import Data.Conduit
import Data.Conduit.TMChan
import qualified Data.Conduit.List as CL

-- Considering doing something with Netwire here, it seems like a good fit
connect :: Link -> Crazyflie ()
connect link = do
    setChannel $ linkChannel link
    setDataRate $ linkDataRate link
    setARC 10
    (input, output) <- liftIO . atomically $ do
        a <- newTBMChan 50
        b <- newTBMChan 50
        return (a,b)
    -- TODO: The rest of setting up the various threads
    return ()

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