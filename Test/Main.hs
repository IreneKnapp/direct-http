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
                 serverParametersUserToChangeTo = Just "irene",
                 serverParametersGroupToChangeTo = Just "irene",
                 serverParametersNames
                   = [(["ireneknapp.com", "localhost"],
                       [(loopbackAddress, 8000, False),
                        (loopbackAddress, 4430, True)])]
               }
             forkIO
             $ do
                 inputData <- httpGet 4096
                 httpPutStr $ "Received " ++ (show $ BS.length inputData) ++ " bytes."
                 return ()
