module Copy (copy) where

import System.Process (runInteractiveProcess)
import System.Info (os)
import System.IO (hPutStr, hClose)
import System.Directory (findExecutable)
import Data.Maybe (isJust)

exec :: String -> [String] -> String -> IO ()
exec bin args text = do
  (inp, _, _, _) <- runInteractiveProcess bin args Nothing Nothing
  hPutStr inp text
  hClose inp

copyMac :: String -> IO ()
copyMac = exec "pbcopy" []

copyLinux :: String -> IO ()
copyLinux content = do
  wlCopyPath <- findExecutable "wl-copy"
  if isJust wlCopyPath then
    exec "wl-copy" [] content
  else
    exec "xclip" ["-selection", "clipboard"] content

copyWindows :: String -> IO ()
copyWindows = exec "clip" []

copy :: String -> IO ()
copy =
  case os of
    "darwin" -> copyMac
    "linux" -> copyLinux
    "windows" -> copyWindows
    _ -> error $ "Unknown os (" ++ os ++ ")"
