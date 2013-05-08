module Network.Crazyflie.Util where

import ClassyPrelude

coerceInt :: (Integral a, Integral b) => a -> b
coerceInt = fromInteger . toInteger