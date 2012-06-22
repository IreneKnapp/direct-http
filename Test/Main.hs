module Main where

import Control.Concurrent
import qualified Data.ByteString as BS
import Network.HTTP
import Network.Socket


main :: IO ()
main = do
  let configuration = [("localhost", 8000, False), ("localhost", 4430, True)]
  addresses <-
    mapM (\(hostname, port, secure) -> do
            addresses <-
              getAddrInfo (Just $ defaultHints {
                                      addrFlags = [AI_ADDRCONFIG,
                                                   AI_NUMERICSERV],
                                      addrSocketType = Stream
                                    })
                          (Just hostname)
                          (Just $ show port)
            return $ zip addresses (repeat secure))
         configuration
    >>= return . concat
  let listenSockets =
        map (\(addressInformation, secure) ->
               let address = addrAddress addressInformation
               in HTTPListenSocketParameters {
                      listenSocketParametersAddress = address,
                      listenSocketParametersSecure = secure
                    })
            addresses
  acceptLoop HTTPServerParameters {
                 serverParametersAccessLogPath = Just "access.log",
                 serverParametersErrorLogPath = Just "error.log",
                 serverParametersDaemonize = True,
                 serverParametersUserToChangeTo = Just "irene",
                 serverParametersGroupToChangeTo = Just "irene",
                 serverParametersForkPrimitive = forkIO,
                 serverParametersListenSockets = listenSockets
               }
             $ do
                 httpPutStr $ "Hello!"
                 -- inputData <- httpGet 4096
                 -- httpPutStr $ "Received " ++ (show $ BS.length inputData) ++ " bytes."
                 return ()
