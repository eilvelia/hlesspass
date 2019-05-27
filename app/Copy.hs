module Copy (copy) where

import System.Process
import System.Info (os)
import System.IO (hPutStr, hClose)

exec :: String -> [String] -> String -> IO ()
exec bin args text = do
  (inp, _, _, _) <- runInteractiveProcess bin args Nothing Nothing
  hPutStr inp text
  hClose inp

copyMac :: String -> IO ()
copyMac = exec "pbcopy" []

copyLinux :: String -> IO ()
copyLinux = exec "xclip" ["-selection", "clipboard"]

copyWindows :: String -> IO ()
copyWindows = exec "clip" []

copy :: String -> IO ()
copy =
  case os of
    "darwin" -> copyMac
    "linux" -> copyLinux
    "windows" -> copyWindows
    _ -> error $ "Unknown os (" ++ os ++ ")"
