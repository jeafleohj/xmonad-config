-- |
module Util.Plugins
  (
    Volume(..),
  ) where

import Xmobar
import System.Process
import Text.Printf (printf)

data Volume = Volume String Int deriving (Read, Show)

instance Exec Volume where
  alias (Volume a _) = a
  rate (Volume _ r) = r
  run _ = uncurry displayVolumen <$> getCurrentVolumenInfo

displayVolumen :: Int -> Bool -> String
displayVolumen percentage isMuted =
  let symbol = case (isMuted, percentage) of
                 (True, _) -> "🔇"
                 (False, p) | p < 33    -> "🔈"
                            | p < 66    -> "🔉"
                            | otherwise -> "🔊"
      percentageStr = if isMuted
                      then replicate 4 ' '
                      else printf "%3d%%" percentage
  in symbol <> percentageStr

getCurrentVolumenInfo :: IO (Int, Bool)
getCurrentVolumenInfo = do
 info <- words  <$> readProcess "pamixer" ["--get-volume", "--get-mute"] ""
 let percentage = read . last $ info
     isMuted = head info == "true"
 return (percentage, isMuted)
