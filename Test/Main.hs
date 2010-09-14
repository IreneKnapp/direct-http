module Main where

import Control.Concurrent
import qualified Data.ByteString as BS
import Network.HTTP
import Network.Socket


main :: IO ()
main = do
  loopbackAddress <- inet_addr "127.0.0.1"
  acceptLoop HTTPServerParameters {
                 serverParametersAccessLogPath = Just "access.log",
                 serverParametersErrorLogPath = Just "error.log",
                 serverParametersDaemonize = True,
                 serverParametersUserToChangeTo = Just "dankna",
                 serverParametersGroupToChangeTo = Just "dankna",
                 serverParametersNames
                   = [(["dankna.com", "localhost"],
                       [(loopbackAddress, 80, False),
                        (loopbackAddress, 443, True)])]
               }
             forkIO
             $ do
                 inputData <- httpGet 4096
                 httpPutStr $ "Received " ++ (show $ BS.length inputData) ++ " bytes."
                 return ()
