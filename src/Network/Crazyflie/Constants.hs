module Network.Crazyflie.Constants
    ( module X
    , cradioVid
    , cradioPid
    , radioChannelRequest
    , radioAddressRequest
    , dataRateRequest
    , radioPowerRequest
    , radioARDRequest
    , radioARCRequest
    , ackEnableRequest
    , contCarrierRequest
    , scanChannelsRequest
    , launchBootloaderRequest
    ) where

import Network.Crazyflie.Types as X
import System.USB as X

cradioVid :: VendorId
cradioVid = 0x1915

cradioPid :: ProductId
cradioPid = 0x7777

radioChannelRequest :: Request
radioChannelRequest = 0x01
radioAddressRequest :: Request
radioAddressRequest = 0x02
dataRateRequest :: Request
dataRateRequest = 0x03
radioPowerRequest :: Request
radioPowerRequest = 0x04
radioARDRequest :: Request
radioARDRequest = 0x05
radioARCRequest :: Request
radioARCRequest = 0x06
ackEnableRequest :: Request
ackEnableRequest = 0x10
contCarrierRequest :: Request
contCarrierRequest = 0x20
scanChannelsRequest :: Request
scanChannelsRequest = 0x21
launchBootloaderRequest :: Request
launchBootloaderRequest = 0xff