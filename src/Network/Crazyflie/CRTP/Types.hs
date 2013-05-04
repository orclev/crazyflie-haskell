module Network.Crazyflie.CRTP.Types
    ( module X
    , Link (..)
    ) where

import Network.Crazyflie.Types as X

data Link = Link {
    linkRadioIndex :: Int,
    linkChannel :: Channel,
    linkDataRate :: DataRate
} deriving Eq

instance Show Link where
    show link = "radio://" ++ index ++ "/" ++ channel++ "/"  ++ rate
        where
            index = show $ linkRadioIndex link
            channel = show $ linkChannel link
            rate = case linkDataRate link of
                    DR_250KPS -> "250K"
                    DR_1MPS -> "1M"
                    DR_2MPS -> "2M"