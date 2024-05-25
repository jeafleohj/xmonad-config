-- | Brightness controll

module Util.Brightness
  ( brightnessUp
  , brightnessDown) where
import XMonad (X)
import XMonad.Util.Run (safeSpawn)

command :: String
command = "light"

brightnessUp :: Int -> X()
brightnessUp n  = do
  safeSpawn command ["-A", show n]

brightnessDown :: Int -> X()
brightnessDown n  = do
  safeSpawn command ["-U", show n]
