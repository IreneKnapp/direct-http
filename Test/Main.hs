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
                 serverParametersForkPrimitive = forkIO,
                 serverParametersListenSockets =
                   [HTTPListenSocketParameters {
                        listenSocketParametersHostAddress = loopbackAddress,
                        listenSocketParametersPortNumber = 8000,
                        listenSocketParametersSecure = False
                      } {- ,
                    HTTPListenSocketParameters {
                        listenSocketParametersHostAddress = loopbackAddress,
                        listenSocketParametersPortNumber = 4430,
                        listenSocketParametersSecure = True
                      } -} ]
               }
             $ do
                 inputData <- httpGet 4096
                 httpPutStr $ "Received " ++ (show $ BS.length inputData) ++ " bytes."
                 return ()
