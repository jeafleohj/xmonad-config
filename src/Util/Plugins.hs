-- |
module Util.Plugins
  (
    Volume(..),
  ) where

import Xmobar
import System.Process
import Text.Printf (printf)
import Control.Exception (SomeException)
import System.IO
import GHC.IO (catch)

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
  info <- words <$> catch (readProcess "pamixer" ["--get-volume", "--get-mute"] "") (handleError "false 0")
  let percentage = read . last $ info
      isMuted = head info == "true"
  return (percentage, isMuted)

handleError :: String -> SomeException -> IO String
handleError defaultValue e = do
  let err = show e
  hPutStrLn stderr $ "Error: " ++ err
  return defaultValue
