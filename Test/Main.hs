module Main where

import Control.Concurrent
import Network.HTTP
import Network.Socket


main :: IO ()
main = do
  loopbackAddress <- inet_addr "127.0.0.1"
  acceptLoop HTTPServerParameters {
                 serverParametersAccessLogPath = Just "access.log",
                 serverParametersErrorLogPath = Just "error.log",
                 serverParametersDaemonize = True,
                 serverParametersNames
                   = [(["dankna.com", "localhost"],
                       [(loopbackAddress, 80, False),
                        (loopbackAddress, 443, True)])]
               }
             forkIO
             $ do
                 httpPutStr $ "Foo."
                 return ()
