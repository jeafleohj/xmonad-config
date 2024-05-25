-- | Config volumen
module Util.Volume
  ( toggleMute,
    raiseVolume,
    lowerVolume,
  )
where

import XMonad (X)
import XMonad.Util.Run (safeSpawn)

command :: String
command = "pamixer"

toggleMute :: X ()
toggleMute = do
  safeSpawn command ["-t"]

raiseVolume :: Int -> X ()
raiseVolume n = do
  safeSpawn command ["-u", "-i", show n]

lowerVolume :: Int -> X ()
lowerVolume n = do
  safeSpawn command ["-u", "-d", show n]
