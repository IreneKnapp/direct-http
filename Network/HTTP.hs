{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}
module Network.HTTP (
             -- * The monad
             HTTP,
             HTTPState,
             MonadHTTP,
             getHTTPState,
             implementationThrowHTTP,
             implementationCatchHTTP,
             implementationBlockHTTP,
             implementationUnblockHTTP,
             
             -- * Accepting requests
             HTTPServerParameters(..),
             acceptLoop,
             
             -- * Logging
             httpLog,
             
             -- * Request information
             -- | It is common practice for web servers to make their own extensions to
             --   the CGI/1.1 set of defined variables.  For example, @REMOTE_PORT@ is
             --   not defined by the specification, but often seen in the wild.
             --   Furthermore, it is also common for user agents to make their own
             --   extensions to the HTTP/1.1 set of defined headers.  Therefore, there
             --   are two levels of call available.  Defined variables and headers may
             --   be interrogated directly, and in addition, there are higher-level
             --   calls which give convenient names and types to the same information.
             --   
             --   Cookies may also be manipulated through HTTP headers directly; the
             --   functions here are provided only as a convenience.
             getRequestVariable,
             getAllRequestVariables,
             Header(..),
             getRequestHeader,
             getAllRequestHeaders,
             Cookie(..),
             getCookie,
             getAllCookies,
             getCookieValue,
             getDocumentRoot,
             getGatewayInterface,
             getPathInfo,
             getPathTranslated,
             getQueryString,
             getRedirectStatus,
             getRedirectURI,
             getRemoteAddress,
             getRemotePort,
             getRemoteHost,
             getRemoteIdent,
             getRemoteUser,
             getRequestMethod,
             getRequestURI,
             getScriptFilename,
             getScriptName,
             getServerAddress,
             getServerName,
             getServerPort,
             getServerProtocol,
             getServerSoftware,
             getAuthenticationType,
             getContentLength,
             getContentType,
             
             -- * Request content data
             -- | At the moment the handler is invoked, all request headers have been
             --   received, but content data has not necessarily been.  Requests to read
             --   content data block the handler (but not other concurrent handlers)
             --   until there is enough data in the buffer to satisfy them, or until
             --   timeout where applicable.
             httpGet,
             httpGetNonBlocking,
             httpGetContents,
             httpIsReadable,
             
             -- * Response information and content data
             -- | When the handler is first invoked, neither response headers nor
             --   content data have been sent to the client.  Setting of response
             --   headers is lazy, merely setting internal variables, until something
             --   forces them to be output.  For example, attempting to send content
             --   data will force response headers to be output first.  It is not
             --   necessary to close the output stream explicitly, but it may be
             --   desirable, for example to continue processing after returning results
             --   to the user.
             --   
             --   There is no reason that client scripts cannot use any encoding they
             --   wish, including the chunked encoding, if they have set appropriate
             --   headers.  This package, however, does not explicitly support that,
             --   because client scripts can easily implement it for themselves.
             --   
             --   At the start of each request, the response status is set to @200 OK@
             --   and the only response header set is @Content-Type: text/html@.  These
             --   may be overridden by later calls, at any time before headers have
             --   been sent.
             --   
             --   Cookies may also be manipulated through HTTP headers directly; the
             --   functions here are provided only as a convenience.
             setResponseStatus,
             getResponseStatus,
             setResponseHeader,
             unsetResponseHeader,
             getResponseHeader,
             setCookie,
             unsetCookie,
             mkSimpleCookie,
             mkCookie,
             permanentRedirect,
             seeOtherRedirect,
             sendResponseHeaders,
             responseHeadersSent,
             responseHeadersModifiable,
             httpPut,
             httpPutStr,
             httpCloseOutput,
             httpIsWritable,
             
             -- * Exceptions
             --   Because it is not possible for user code to enter the HTTP monad
             --   from outside it, catching exceptions in IO will not work.  Therefore
             --   a full set of exception primitives designed to work with any
             --   'MonadHTTP' instance is provided.
             HTTPException(..),
             httpThrow,
             httpCatch,
             httpBlock,
             httpUnblock,
             httpBracket,
             httpFinally,
             httpTry,
             httpHandle,
             httpOnException
            )
    where

import Control.Concurrent
import qualified Control.Exception as Exception
import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Typeable
import Data.Word
import Foreign.C.Error
import GHC.IO.Exception (IOErrorType(..))
import qualified Network.Socket as Network hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Network
import Prelude hiding (catch)
import System.Daemonize
import System.Environment
import System.Exit
import System.IO
import System.IO.Error (ioeGetErrorType)
import qualified System.IO.Error as System
import System.Locale (defaultTimeLocale)
import qualified System.Posix as POSIX


-- | An opaque type representing the state of the HTTP server during a single
--   connection from a client.
data HTTPState = HTTPState {
    httpStateAccessLogMaybeHandleMVar :: MVar (Maybe Handle),
    httpStateErrorLogMaybeHandleMVar :: MVar (Maybe Handle),
    httpStateMaybeConnection :: Maybe HTTPConnection
  }


data HTTPConnection = HTTPConnection {
    httpConnectionSocket :: Network.Socket,
    httpConnectionPeer :: Network.SockAddr,
    httpConnectionInputBufferMVar :: MVar ByteString,
    httpConnectionTimestamp :: MVar POSIXTime,
    httpConnectionQueryString :: MVar (Maybe String),
    httpConnectionRemoteHostname :: MVar (Maybe (Maybe String)),
    httpConnectionRequestMethod :: MVar String,
    httpConnectionRequestURI :: MVar String,
    httpConnectionRequestProtocol :: MVar String,
    httpConnectionRequestHeaderMap :: MVar (Map Header ByteString),
    httpConnectionRequestContentBuffer :: MVar ByteString,
    httpConnectionRequestContentParameters :: MVar RequestContentParameters,
    httpConnectionResponseHeadersSent :: MVar Bool,
    httpConnectionResponseHeadersModifiable :: MVar Bool,
    httpConnectionResponseStatus :: MVar Int,
    httpConnectionResponseHeaderMap :: MVar (Map Header ByteString),
    httpConnectionResponseCookieMap :: MVar (Map String Cookie),
    httpConnectionResponseContentBuffer :: MVar ByteString,
    httpConnectionResponseContentParameters :: MVar ResponseContentParameters
  }


data RequestContentParameters
  = RequestContentUninitialized
  | RequestContentNone
  | RequestContentClosed
  | RequestContentIdentity Int
  | RequestContentChunked Bool Int

data ResponseContentParameters
  = ResponseContentUninitialized
  | ResponseContentClosed
  | ResponseContentBufferedIdentity
  | ResponseContentUnbufferedIdentity Int
  | ResponseContentChunked


data Cookie = Cookie {
    cookieName :: String,
    cookieValue :: String,
    cookieVersion :: Int,
    cookiePath :: Maybe String,
    cookieDomain :: Maybe String,
    cookieMaxAge :: Maybe Int,
    cookieSecure :: Bool,
    cookieComment :: Maybe String
  } deriving (Show)


data ConnectionTerminatingError = UnexpectedEndOfInput
                                  deriving (Typeable)


instance Exception.Exception ConnectionTerminatingError


instance Show ConnectionTerminatingError where
  show UnexpectedEndOfInput = "Unexpected end of input."


-- | The monad within which each single request from a client is handled.
type HTTP = ReaderT HTTPState IO


-- | The class of monads within which the HTTP calls are valid.  You may wish to
--   create your own monad implementing this class.
class (MonadIO m) => MonadHTTP m where
    -- | Returns the opaque 'HTTPState' object representing the state of
    --   the HTTP server.
    --   Should not be called directly by user code, except implementations of
    --   'MonadHTTP'; exported so that
    --   user monads can implement the interface.
    getHTTPState
        :: m HTTPState
    -- | Throws an exception in the monad.
    --   Should not be called directly by user code; exported so that
    --   user monads can implement the interface.  See 'httpThrow'.
    implementationThrowHTTP
        :: (Exception.Exception e)
        => e -- ^ The exception to throw
        -> m a
    -- | Perform an action in the monad, with a given exception-handler action bound.
    --   Should not be called directly by user code; exported so that
    --   user monads can implement the interface.  See 'httpCatch'.
    implementationCatchHTTP
        :: (Exception.Exception e)
        => m a -- ^ The action to run with the exception handler binding in scope.
        -> (e -> m a) -- ^ The exception handler to bind.
        -> m a
    -- | Block exceptions within an action.
    --   Should not be called directly by user code; exported so that
    --   user monads can implement the interface.  See 'httpBlock'.
    implementationBlockHTTP
        :: m a -- ^ The action to run with exceptions blocked.
        -> m a
    -- | Unblock exceptions within an action.
    --   Should not be called directly by user code; exported so that
    --   user monads can implement the interface.  See 'httpUnblock'.
    implementationUnblockHTTP
        :: m a -- ^ The action to run with exceptions unblocked.
        -> m a


instance MonadHTTP HTTP where
    getHTTPState = ask
    implementationThrowHTTP exception = liftIO $ Exception.throwIO exception
    implementationCatchHTTP action handler = do
      state <- getHTTPState
      liftIO $ Exception.catch (runReaderT action state)
                               (\exception -> do
                                  runReaderT (handler exception) state)
    implementationBlockHTTP action = do
      state <- getHTTPState
      liftIO $ Exception.block (runReaderT action state)
    implementationUnblockHTTP action = do
      state <- getHTTPState
      liftIO $ Exception.unblock (runReaderT action state)


getHTTPConnection :: (MonadHTTP m) => m HTTPConnection
getHTTPConnection = do
  HTTPState { httpStateMaybeConnection = maybeConnection } <- getHTTPState
  return $ fromJust maybeConnection


-- | A record used to configure the server.  Contains optional paths to files
--   for the access and error logs (if these are omitted, logging is not done),
--   and a flag indicating whether to run as a daemon.  Also contains the names
--   field, which is a list containing one entry for each distinct server; each
--   server entry consists of a list of names to recognize, the first such name
--   being the canonical one, and a list of addresses and ports to bind to, with
--   a flag for each indicating whether the binding should use the secure
--   version of the protocol.
data HTTPServerParameters = HTTPServerParameters {
    serverParametersAccessLogPath :: Maybe FilePath,
    serverParametersErrorLogPath :: Maybe FilePath,
    serverParametersDaemonize :: Bool,
    serverParametersUserToChangeTo :: Maybe String,
    serverParametersGroupToChangeTo :: Maybe String,
    serverParametersNames :: [([Network.HostName],
                               [(Network.HostAddress,
                                 Network.PortNumber,
                                 Bool)])]
  }


-- | Takes a server parameters record, a forking primitive such as 'forkIO'
--   or 'forkOS', and a handler, and concurrently accepts requests from user
--   agents, forking with the primitive and invoking the handler in the forked
--   thread inside the 'HTTP' monad for each one.
--   
--   If the access logfile path is not Nothing, opens this logfile in append
--   mode and uses it to log all accesses; otherwise, access is not logged.
--   
--   If the error logfile path is not Nothing, opens this logfile in append mode
--   and uses it to log all errors; otherwise, if not daemonizing, errors are
--   logged to standard output; if daemonizing, errors are not logged.
--   
--   If the daemonize flag is True, closes the standard IO streams and moves
--   the process into the background, doing all the usual Unix things to make it
--   run as a daemon henceforth.  This is optional because it might be useful to
--   turn it off for debugging purposes.
--   
--   It is valid to use a custom forking primitive, such as one that attempts to
--   pool OS threads, but the primitive must actually provide concurrency -
--   otherwise there will be a deadlock.  There is no support for
--   single-threaded operation.
--   
--   Note that although there is no mechanism to substitute another type of
--   monad for HTTP, you can enter your own monad within the handler, much as
--   you would enter your own monad within IO.  You simply have to implement
--   the 'MonadHTTP' class.
--   
--   Any exceptions not caught within the handler are caught by
--   'acceptLoop', and cause the termination of that handler, but not
--   of the connection or the accept loop.
acceptLoop
    :: HTTPServerParameters
    -- ^ Parameters describing the behavior of the server to run.
    -> (IO () -> IO ThreadId)
    -- ^ A forking primitive, typically either 'forkIO' or 'forkOS'.
    -> (HTTP ())
    -- ^ A handler which is invoked once for each incoming connection.
    -> IO ()
    -- ^ Never actually returns.
acceptLoop parameters fork handler = do
  (listenSocket, accessLogMaybeHandle, errorLogMaybeHandle)
    <- Exception.catch
       (do
         listenSocket <- createListenSocket
         accessLogMaybeHandle
           <- case serverParametersAccessLogPath parameters of
                Nothing -> return Nothing
                Just path -> openBinaryFile path AppendMode
                             >>= return . Just
         errorLogMaybeHandle
           <- case serverParametersErrorLogPath parameters of
                Nothing -> if serverParametersDaemonize parameters
                             then return Nothing
                             else return $ Just stdout
                Just path -> openBinaryFile path AppendMode
                             >>= return . Just
         return (listenSocket, accessLogMaybeHandle, errorLogMaybeHandle))
        (\e -> do
           hPutStrLn stderr
                     $ "Failed to start: "
                       ++ (show (e :: Exception.SomeException))
           exitFailure)
  accessLogMaybeHandleMVar <- newMVar accessLogMaybeHandle
  errorLogMaybeHandleMVar <- newMVar errorLogMaybeHandle
  if serverParametersDaemonize parameters
    then do
      let maybeHandleMVarToFd maybeHandleMVar = do
            maybeHandle <- takeMVar maybeHandleMVar
            case maybeHandle of
              Nothing -> do
                putMVar maybeHandleMVar Nothing
                return Nothing
              Just handle -> do
                fd <- POSIX.handleToFd handle
                handle <- POSIX.fdToHandle fd
                putMVar maybeHandleMVar $ Just handle
                return $ Just fd
      accessLogMaybeFd <- maybeHandleMVarToFd accessLogMaybeHandleMVar
      errorLogMaybeFd <- maybeHandleMVarToFd errorLogMaybeHandleMVar
      listenSocketMaybeFd <- return $ Just
                                    $ POSIX.Fd $ Network.fdSocket listenSocket
      daemonize $ defaultDaemonOptions {
                    daemonFileDescriptorsToLeaveOpen
                      = (map fromJust $ filter isJust [accessLogMaybeFd,
                                                       errorLogMaybeFd,
                                                       listenSocketMaybeFd]),
                    daemonUserToChangeTo
                      = serverParametersUserToChangeTo parameters,
                    daemonGroupToChangeTo
                      = serverParametersGroupToChangeTo parameters
                  }
    else return ()
  let state = HTTPState {
                httpStateAccessLogMaybeHandleMVar
                  = accessLogMaybeHandleMVar,
                httpStateErrorLogMaybeHandleMVar
                  = errorLogMaybeHandleMVar,
                httpStateMaybeConnection = Nothing
              }
  let acceptLoop' listenSocket = do
        (socket, peer) <- liftIO $ Network.accept listenSocket
        state <- ask
        liftIO $ fork $ flip runReaderT state
                        $ requestLoop socket peer handler
        acceptLoop' listenSocket
  flip runReaderT state $ do
    httpLog $ "Server started."
    acceptLoop' listenSocket


createListenSocket :: IO Network.Socket
createListenSocket = do
  listenSocket <- Network.socket Network.AF_INET
                                 Network.Stream
                                 Network.defaultProtocol
  Network.bindSocket listenSocket $ Network.SockAddrInet 80 Network.iNADDR_ANY
  Network.listen listenSocket 1024
  return listenSocket


requestLoop :: Network.Socket
            -> Network.SockAddr
            -> HTTP ()
            -> HTTP ()
requestLoop socket peer handler = do
  inputBufferMVar <- liftIO $ newMVar $ BS.empty
  timestampMVar <- liftIO $ newEmptyMVar
  queryStringMVar <- liftIO $ newEmptyMVar
  remoteHostnameMVar <- liftIO $ newMVar Nothing
  requestMethodMVar <- liftIO $ newEmptyMVar
  requestURIMVar <- liftIO $ newEmptyMVar
  requestProtocolMVar <- liftIO $ newEmptyMVar
  requestHeaderMapMVar <- liftIO $ newEmptyMVar
  requestContentBufferMVar <- liftIO $ newEmptyMVar
  requestContentParametersMVar <- liftIO $ newEmptyMVar
  responseHeadersSentMVar <- liftIO $ newEmptyMVar
  responseHeadersModifiableMVar <- liftIO $ newEmptyMVar
  responseStatusMVar <- liftIO $ newEmptyMVar
  responseHeaderMapMVar <- liftIO $ newEmptyMVar
  responseCookieMapMVar <- liftIO $ newEmptyMVar
  responseContentBufferMVar <- liftIO $ newEmptyMVar
  responseContentParametersMVar <- liftIO $ newEmptyMVar
  let connection = HTTPConnection {
                     httpConnectionSocket = socket,
                     httpConnectionPeer = peer,
                     httpConnectionInputBufferMVar = inputBufferMVar,
                     httpConnectionTimestamp = timestampMVar,
                     httpConnectionQueryString = queryStringMVar,
                     httpConnectionRemoteHostname = remoteHostnameMVar,
                     httpConnectionRequestMethod = requestMethodMVar,
                     httpConnectionRequestURI = requestURIMVar,
                     httpConnectionRequestProtocol = requestProtocolMVar,
                     httpConnectionRequestHeaderMap = requestHeaderMapMVar,
                     httpConnectionRequestContentBuffer
                       = requestContentBufferMVar,
                     httpConnectionRequestContentParameters
                       = requestContentParametersMVar,
                     httpConnectionResponseHeadersSent
                       = responseHeadersSentMVar,
                     httpConnectionResponseHeadersModifiable
                       = responseHeadersModifiableMVar,
                     httpConnectionResponseStatus = responseStatusMVar,
                     httpConnectionResponseHeaderMap = responseHeaderMapMVar,
                     httpConnectionResponseCookieMap = responseCookieMapMVar,
                     httpConnectionResponseContentBuffer
                       = responseContentBufferMVar,
                     httpConnectionResponseContentParameters
                       = responseContentParametersMVar
                   }
      requestLoop' = do
        httpCatch requestLoop''
                  (\error -> do
                     httpLog $ "Internal uncaught exception: "
                               ++ (show (error :: Exception.SomeException))
                     liftIO $ Network.sClose socket)
      requestLoop'' = do
        httpCatch requestLoop'''
                  (\error -> do
                     HTTPConnection {
                         httpConnectionPeer = Network.SockAddrInet _ address
                       } <- getHTTPConnection
                     peerString <- liftIO $ Network.inet_ntoa address
                     httpLog $ "Connection from " ++ peerString
                               ++ " terminated due to error: "
                               ++ (show (error :: ConnectionTerminatingError))
                     liftIO $ Network.sClose socket)
      requestLoop''' = do
        maybeRequestInfo <- recvHeaders
        case maybeRequestInfo of
          Nothing -> do
            liftIO $ Exception.catch (Network.sClose socket)
                                     (\error -> do
                                        return $ error
                                          :: IO Exception.IOException
                                        return ())
            return ()
          Just (method, url, protocol, headers) -> do
            timestamp <- liftIO $ getPOSIXTime
            liftIO $ putMVar timestampMVar timestamp
            liftIO $ putMVar queryStringMVar Nothing -- TODO
            liftIO $ putMVar requestMethodMVar $ UTF8.toString method
            liftIO $ putMVar requestURIMVar $ UTF8.toString url
            liftIO $ putMVar requestProtocolMVar $ UTF8.toString protocol
            liftIO $ putMVar requestHeaderMapMVar headers
            liftIO $ putMVar requestContentBufferMVar BS.empty
            liftIO $ putMVar requestContentParametersMVar
                             RequestContentUninitialized
            liftIO $ putMVar responseHeadersSentMVar False
            liftIO $ putMVar responseHeadersModifiableMVar True
            liftIO $ putMVar responseStatusMVar 200
            liftIO $ putMVar responseHeaderMapMVar Map.empty
            liftIO $ putMVar responseCookieMapMVar Map.empty
            liftIO $ putMVar responseContentBufferMVar BS.empty
            liftIO $ putMVar responseContentParametersMVar
                             ResponseContentUninitialized
            httpCatch
              (do
                valid <- getRequestValid
                if valid
                  then do
                    prepareResponse
                    handler
                  else do
                    setResponseStatus 400)
              (\error -> do
                 httpLog $ "Uncaught exception: "
                           ++ (show (error :: Exception.SomeException))
                 alreadySent <- responseHeadersSent
                 if alreadySent
                   then return ()
                   else setResponseStatus 500)
            logAccess
            isWritable <- httpIsWritable
            if isWritable
              then httpCloseOutput
              else return ()
            liftIO $ takeMVar timestampMVar
            liftIO $ takeMVar queryStringMVar
            liftIO $ takeMVar requestMethodMVar
            liftIO $ takeMVar requestURIMVar
            liftIO $ takeMVar requestProtocolMVar
            liftIO $ takeMVar requestHeaderMapMVar
            liftIO $ takeMVar requestContentBufferMVar
            liftIO $ takeMVar requestContentParametersMVar
            liftIO $ takeMVar responseHeadersSentMVar
            liftIO $ takeMVar responseHeadersModifiableMVar
            liftIO $ takeMVar responseStatusMVar
            liftIO $ takeMVar responseHeaderMapMVar
            liftIO $ takeMVar responseCookieMapMVar
            liftIO $ takeMVar responseContentBufferMVar
            liftIO $ takeMVar responseContentParametersMVar
            requestLoop'''
  state <- ask
  liftIO $ flip runReaderT
                (state { httpStateMaybeConnection = Just connection })
                requestLoop'


getRequestValid :: (MonadHTTP m) => m Bool
getRequestValid = do
  hasContent <- getRequestHasContent
  let getHeadersValid = do
        HTTPConnection { httpConnectionRequestHeaderMap = mvar }
          <- getHTTPConnection
        headerMap <- liftIO $ readMVar mvar
        return $ all (\header -> (isValidInRequest header)
                                 && (hasContent
                                     || (not $ isValidOnlyWithEntity header)))
                     $ Map.keys headerMap
      getContentValid = do
        contentAllowed <- getRequestContentAllowed
        return $ contentAllowed || not hasContent
  httpVersion <- getRequestProtocol >>= return . fromJust
  case httpVersion of
    "HTTP/1.0" -> do
      headersValid <- getHeadersValid
      contentValid <- getContentValid
      return $ and [headersValid, contentValid]
    "HTTP/1.1" -> do
      headersValid <- getHeadersValid
      contentValid <- getContentValid
      mandatoryHeadersIncluded <- do
        maybeHost <- getRequestHeader HttpHost
        case maybeHost of
          Nothing -> return False
          Just host -> return True
      return $ and [headersValid, mandatoryHeadersIncluded, contentValid]


prepareResponse :: (MonadHTTP m) => m ()
prepareResponse = do
  HTTPConnection { httpConnectionTimestamp = mvar } <- getHTTPConnection
  timestamp <- liftIO $ readMVar mvar
  let dateString = formatTime defaultTimeLocale
                              "%a, %d %b %Y %H:%M:%S Z"
                              $ posixSecondsToUTCTime timestamp
  setResponseHeader HttpDate dateString
  setResponseHeader HttpContentType "text/html; charset=UTF8"


logAccess :: (MonadHTTP m) => m ()
logAccess = do
  maybePeerString <- getRemoteHost
  peerString <- case maybePeerString of
                  Nothing -> do
                    HTTPConnection { httpConnectionPeer = peer } <- getHTTPConnection
                    case peer of
                      Network.SockAddrInet _ address
                        -> liftIO $ Network.inet_ntoa address
                  Just peerString -> return peerString
  identString <- return "-"
  usernameString <- return "-"
  HTTPConnection { httpConnectionTimestamp = timestampMVar } <- getHTTPConnection
  timestamp <- liftIO $ readMVar timestampMVar
  let timestampString = formatTime defaultTimeLocale
                                   "%-d/%b/%Y:%H:%M:%S %z"
                                   $ posixSecondsToUTCTime timestamp
  methodString <- getRequestMethod >>= return . fromJust
  urlString <- getRequestURI >>= return . fromJust
  protocolString <- getRequestProtocol >>= return . fromJust
  responseStatusString <- getResponseStatus >>= return . show
  maybeResponseSize <- return (Nothing :: Maybe Int) -- TODO
  responseSizeString
    <- case maybeResponseSize of
         Nothing -> return "-"
         Just responseSize -> return $ show responseSize
  maybeReferrerString <- getRequestHeader HttpReferrer
  referrerString <- case maybeReferrerString of
                      Nothing -> return "-"
                      Just referrerString -> return referrerString
  maybeUserAgentString <- getRequestHeader HttpUserAgent
  userAgentString <- case maybeUserAgentString of
                      Nothing -> return "-"
                      Just userAgentString -> return userAgentString
  httpAccessLog $ peerString
                  ++ " "
                  ++ identString
                  ++ " "
                  ++ usernameString
                  ++ " ["
                  ++ timestampString
                  ++ "] \""
                  ++ methodString
                  ++ " "
                  ++ urlString
                  ++ " "
                  ++ protocolString
                  ++ "\" "
                  ++ responseStatusString
                  ++ " "
                  ++ responseSizeString
                  ++ " \""
                  ++ referrerString
                  ++ "\" \""
                  ++ userAgentString
                  ++ "\""


parseCookies :: String -> [Cookie]
parseCookies value =
  let findSeparator string
          = let quotePoint = if (length string > 0) && (string !! 0 == '"')
                               then 1 + (findBalancingQuote $ drop 1 string)
                               else 0
                maybeSemicolonPoint
                    = case (findIndex (\c -> (c == ';') || (c == ','))
                                          $ drop quotePoint string)
                      of Nothing -> Nothing
                         Just index -> Just $ index + quotePoint
            in maybeSemicolonPoint
      findBalancingQuote string
          = let consume accumulator ('\\' : c : rest) = consume (accumulator + 2) rest
                consume accumulator ('"' : rest) = accumulator
                consume accumulator (c : rest) = consume (accumulator + 1) rest
                consume accumulator "" = accumulator
            in consume 0 string
      split [] = []
      split string = case findSeparator string of
                       Nothing -> [string]
                       Just index ->
                         let (first, rest) = splitAt index string
                         in first : (split $ drop 1 rest)
      splitNameValuePair string = case elemIndex '=' (filterNameValuePair string) of
                                    Nothing -> (string, "")
                                    Just index -> let (first, rest)
                                                          = splitAt index
                                                                    (filterNameValuePair
                                                                      string)
                                                  in (first, filterValue (drop 1 rest))
      filterNameValuePair string
          = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace string
      filterValue string = if (length string > 0) && ((string !! 0) == '"')
                             then take (findBalancingQuote $ drop 1 string)
                                       $ drop 1 string
                             else string
      pairs = map splitNameValuePair $ split value
      (version, pairs') = case pairs of
                                 ("$Version", versionString) : rest
                                     -> case parseInt versionString of
                                          Nothing -> (0, rest)
                                          Just version -> (version, rest)
                                 _ -> (0, pairs)
      takeCookie pairs = case pairs of
                           (name, value) : pairs'
                               | (length name > 0) && (take 1 name /= "$")
                                    -> let (maybePath, maybeDomain, pairs'')
                                               = takePathAndDomain pairs'
                                       in (Cookie {
                                             cookieName = name,
                                             cookieValue = value,
                                             cookieVersion = version,
                                             cookiePath = maybePath,
                                             cookieDomain = maybeDomain,
                                             cookieMaxAge = Nothing,
                                             cookieSecure = False,
                                             cookieComment = Nothing
                                           }
                                           : takeCookie pairs'')
                           _ : pairs' -> takeCookie pairs'
                           [] -> []
      takePathAndDomain pairs = let (maybePath, pairs')
                                        = case pairs of ("$Path", path) : rest
                                                            -> (Just path, rest)
                                                        _ -> (Nothing, pairs)
                                    (maybeDomain, pairs'')
                                        = case pairs' of ("$Domain", domain) : rest
                                                             -> (Just domain, rest)
                                                         _ -> (Nothing, pairs')
                                in (maybePath, maybeDomain, pairs'')
  in takeCookie pairs'


printCookies :: [Cookie] -> ByteString
printCookies cookies =
    let printCookie cookie
            = BS.intercalate (UTF8.fromString ";")
                             $ map printNameValuePair $ nameValuePairs cookie
        printNameValuePair (name, Nothing) = UTF8.fromString name
        printNameValuePair (name, Just value)
            = BS.concat [UTF8.fromString name,
                         UTF8.fromString "=",
                         UTF8.fromString value]
        {- Safari doesn't like this.
            = if isValidCookieToken value
                then name ++ "=" ++ value
                else name ++ "=\"" ++ escape value ++ "\""
        escape "" = ""
        escape ('\\':rest) = "\\\\" ++ escape rest
        escape ('\"':rest) = "\\\"" ++ escape rest
        escape (c:rest) = [c] ++ escape rest
         -}
        nameValuePairs cookie = [(cookieName cookie, Just $ cookieValue cookie)]
                                ++ (case cookieComment cookie of
                                      Nothing -> []
                                      Just comment -> [("Comment", Just comment)])
                                ++ (case cookieDomain cookie of
                                      Nothing -> []
                                      Just domain -> [("Domain", Just domain)])
                                ++ (case cookieMaxAge cookie of
                                      Nothing -> []
                                      Just maxAge -> [("Max-Age", Just $ show maxAge)])
                                ++ (case cookiePath cookie of
                                      Nothing -> []
                                      Just path -> [("Path", Just $ path)])
                                ++ (case cookieSecure cookie of
                                      False -> []
                                      True -> [("Secure", Nothing)])
                                ++ [("Version", Just $ show $ cookieVersion cookie)]
    in BS.intercalate (UTF8.fromString ",") $ map printCookie cookies


parseInt :: String -> Maybe Int
parseInt string =
    if (not $ null string) && (all isDigit string)
      then Just $ let accumulate "" accumulator = accumulator
                      accumulate (n:rest) accumulator
                                 = accumulate rest $ accumulator * 10 + digitToInt n
                  in accumulate string 0
      else Nothing


recvHeaders :: (MonadHTTP m)
               => m (Maybe (ByteString,
                            ByteString,
                            ByteString,
                            Map Header ByteString))
recvHeaders = do
  HTTPConnection { httpConnectionInputBufferMVar = inputBufferMVar }
    <- getHTTPConnection
  inputBuffer <- liftIO $ takeMVar inputBufferMVar
  (inputBuffer, maybeLine) <- recvLine inputBuffer
  result <- case maybeLine of
    Nothing -> return Nothing
    Just line -> do
      let computeWords input =
            let (before, after) = BS.breakSubstring (UTF8.fromString " ") input
            in if BS.null after
              then [before]
              else let rest = computeWords $ BS.drop 1 after
                   in before : rest
          words = computeWords line
      case words of
        [method, url, protocol]
          | (isValidMethod method)
            && (isValidURL url)
            && (isValidProtocol protocol)
          -> do
            let loop inputBuffer headersSoFar = do
                  (inputBuffer, maybeLine) <- recvLine inputBuffer
                  case maybeLine of
                    Nothing -> return Nothing
                    Just line
                      | BS.null line -> do
                          return $ Just (method, url, protocol, headersSoFar)
                      | otherwise -> do
                          case parseHeader line of
                            Nothing -> do
                              logInvalidRequest
                              return Nothing
                            Just (header, value) -> do
                              let headersSoFar'
                                   = case Map.lookup header headersSoFar of
                                       Nothing -> Map.insert header
                                                             value
                                                             headersSoFar
                                       Just oldValue
                                         -> Map.insert
                                            header
                                            (BS.concat [oldValue,
                                                        (UTF8.fromString ","),
                                                        value])
                                            headersSoFar
                              loop inputBuffer headersSoFar'
            loop inputBuffer Map.empty
        _ -> do
          logInvalidRequest
          return Nothing
  liftIO $ putMVar inputBufferMVar inputBuffer
  return result


parseHeader :: ByteString -> Maybe (Header, ByteString)
parseHeader line = do
  case BS.breakSubstring (UTF8.fromString ":") line of
    (_, bytestring) | bytestring == BS.empty -> Nothing
    (name, delimitedValue) -> Just (toHeader name, BS.drop 1 delimitedValue)


logInvalidRequest :: MonadHTTP m => m ()
logInvalidRequest = do
  HTTPConnection { httpConnectionPeer = Network.SockAddrInet _ address } <- getHTTPConnection
  peerString <- liftIO $ Network.inet_ntoa address
  httpLog $ "Invalid request from " ++ peerString ++ "; closing its connection."


isValidMethod :: ByteString -> Bool
isValidMethod bytestring
  | bytestring == UTF8.fromString "OPTIONS" = True
  | bytestring == UTF8.fromString "GET" = True
  | bytestring == UTF8.fromString "HEAD" = True
  | bytestring == UTF8.fromString "POST" = True
  | bytestring == UTF8.fromString "PUT" = True
  | bytestring == UTF8.fromString "DELETE" = True
  | bytestring == UTF8.fromString "TRACE" = True
  | bytestring == UTF8.fromString "CONNECT" = True
  | otherwise = False


isValidURL :: ByteString -> Bool
isValidURL _ = True


isValidProtocol :: ByteString -> Bool
isValidProtocol bytestring
  | bytestring == UTF8.fromString "HTTP/1.0" = True
  | bytestring == UTF8.fromString "HTTP/1.1" = True
  | otherwise = False


recvLine :: (MonadHTTP m) => ByteString -> m (ByteString, Maybe ByteString)
recvLine inputBuffer = do
  let loop inputBuffer length firstIteration = do
        let blocking = not firstIteration
        (inputBuffer, endOfInput)
          <- extendInputBuffer inputBuffer length blocking
        let (before, after)
              = BS.breakSubstring (UTF8.fromString "\r\n") inputBuffer
        if BS.null after
          then if endOfInput
                 then return (inputBuffer, Nothing)
                 else loop inputBuffer (length + 80) False
          else return (BS.drop 2 after, Just before)
  let (before, after)
        = BS.breakSubstring (UTF8.fromString "\r\n") inputBuffer
  if BS.null after
    then loop inputBuffer 80 True
    else return (BS.drop 2 after, Just before)


recvBlock :: (MonadHTTP m) => Int -> m ByteString
recvBlock length = do
  HTTPConnection { httpConnectionInputBufferMVar = inputBufferMVar } <- getHTTPConnection
  inputBuffer <- liftIO $ takeMVar inputBufferMVar
  (inputBuffer, endOfInput)
    <- extendInputBuffer inputBuffer length True
  (result, inputBuffer) <- return $ BS.splitAt length inputBuffer
  liftIO $ putMVar inputBufferMVar inputBuffer
  return result


extendInputBuffer :: (MonadHTTP m)
                  => ByteString -> Int -> Bool -> m (ByteString, Bool)
extendInputBuffer inputBuffer length blocking = do
  HTTPConnection { httpConnectionSocket = socket } <- getHTTPConnection
  let loop inputBuffer = do
        if BS.length inputBuffer < length
          then do
            newInput <- liftIO $ Network.recv socket 4096
            if BS.null newInput
              then return (inputBuffer, True)
              else if blocking
                     then loop $ BS.append inputBuffer newInput
                     else return (BS.append inputBuffer newInput, False)
          else return (inputBuffer, False)
  loop inputBuffer


-- | Logs a message using the web server's logging facility.
httpLog :: (MonadHTTP m) => String -> m ()
httpLog message = do
  HTTPState { httpStateErrorLogMaybeHandleMVar = logMVar } <- getHTTPState
  httpBracket (liftIO $ takeMVar logMVar)
              (\maybeHandle -> liftIO $ putMVar logMVar maybeHandle)
              (\maybeHandle -> do
                 case maybeHandle of
                   Nothing -> return ()
                   Just handle -> do
                     timestamp <- liftIO $ getPOSIXTime
                     let timestampString
                           = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                                        $ posixSecondsToUTCTime timestamp
                     liftIO $ hPutStrLn handle $ timestampString ++ " " ++ message
                     liftIO $ hFlush handle)


-- | Logs a message using the web server's logging facility.
httpAccessLog :: (MonadHTTP m) => String -> m ()
httpAccessLog message = do
  HTTPState { httpStateAccessLogMaybeHandleMVar = logMVar } <- getHTTPState
  liftIO $ withMVar logMVar
                    (\maybeHandle -> case maybeHandle of
                                       Nothing -> return ()
                                       Just handle -> do
                                         hPutStrLn handle message
                                         hFlush handle)


-- | Headers are classified by HTTP/1.1 as request headers, response headers,
--   entity headers, or general headers.
data Header
    -- | Request headers
    = HttpAccept
    | HttpAcceptCharset
    | HttpAcceptEncoding
    | HttpAcceptLanguage
    | HttpAuthorization
    | HttpExpect
    | HttpFrom
    | HttpHost
    | HttpIfMatch
    | HttpIfModifiedSince
    | HttpIfNoneMatch
    | HttpIfRange
    | HttpIfUnmodifiedSince
    | HttpMaxForwards
    | HttpProxyAuthorization
    | HttpRange
    | HttpReferrer
    | HttpTE
    | HttpUserAgent
    -- | Response headers
    | HttpAcceptRanges
    | HttpAge
    | HttpETag
    | HttpLocation
    | HttpProxyAuthenticate
    | HttpRetryAfter
    | HttpServer
    | HttpVary
    | HttpWWWAuthenticate
    -- | Entity headers
    | HttpAllow
    | HttpContentEncoding
    | HttpContentLanguage
    | HttpContentLength
    | HttpContentLocation
    | HttpContentMD5
    | HttpContentRange
    | HttpContentType
    | HttpExpires
    | HttpLastModified
    | HttpExtensionHeader ByteString
    -- | General headers
    | HttpCacheControl
    | HttpConnection
    | HttpDate
    | HttpPragma
    | HttpTrailer
    | HttpTransferEncoding
    | HttpUpgrade
    | HttpVia
    | HttpWarning
    -- | Nonstandard headers
    | HttpCookie
    | HttpSetCookie
      deriving (Eq, Ord)


instance Show Header where 
    show header = UTF8.toString $ fromHeader header


data HeaderType = RequestHeader
                | ResponseHeader
                | EntityHeader
                | GeneralHeader
                  deriving (Eq, Show)


headerType :: Header -> HeaderType
headerType HttpAccept = RequestHeader
headerType HttpAcceptCharset = RequestHeader
headerType HttpAcceptEncoding = RequestHeader
headerType HttpAcceptLanguage = RequestHeader
headerType HttpAuthorization = RequestHeader
headerType HttpExpect = RequestHeader
headerType HttpFrom = RequestHeader
headerType HttpHost = RequestHeader
headerType HttpIfMatch = RequestHeader
headerType HttpIfModifiedSince = RequestHeader
headerType HttpIfNoneMatch = RequestHeader
headerType HttpIfRange = RequestHeader
headerType HttpIfUnmodifiedSince = RequestHeader
headerType HttpMaxForwards = RequestHeader
headerType HttpProxyAuthorization = RequestHeader
headerType HttpRange = RequestHeader
headerType HttpReferrer = RequestHeader
headerType HttpTE = RequestHeader
headerType HttpUserAgent = RequestHeader
headerType HttpAcceptRanges = ResponseHeader
headerType HttpAge = ResponseHeader
headerType HttpETag = ResponseHeader
headerType HttpLocation = ResponseHeader
headerType HttpProxyAuthenticate = ResponseHeader
headerType HttpRetryAfter = ResponseHeader
headerType HttpServer = ResponseHeader
headerType HttpVary = ResponseHeader
headerType HttpWWWAuthenticate = ResponseHeader
headerType HttpAllow = EntityHeader
headerType HttpContentEncoding = EntityHeader
headerType HttpContentLanguage = EntityHeader
headerType HttpContentLength = EntityHeader
headerType HttpContentLocation = EntityHeader
headerType HttpContentMD5 = EntityHeader
headerType HttpContentRange = EntityHeader
headerType HttpContentType = EntityHeader
headerType HttpExpires = EntityHeader
headerType HttpLastModified = EntityHeader
headerType (HttpExtensionHeader _) = EntityHeader
headerType HttpCacheControl = GeneralHeader
headerType HttpConnection = GeneralHeader
headerType HttpDate = GeneralHeader
headerType HttpPragma = GeneralHeader
headerType HttpTrailer = GeneralHeader
headerType HttpTransferEncoding = GeneralHeader
headerType HttpUpgrade = GeneralHeader
headerType HttpVia = GeneralHeader
headerType HttpWarning = GeneralHeader
headerType HttpCookie = RequestHeader
headerType HttpSetCookie = ResponseHeader


fromHeader :: Header -> ByteString
fromHeader HttpAccept = UTF8.fromString "Accept"
fromHeader HttpAcceptCharset = UTF8.fromString "Accept-Charset"
fromHeader HttpAcceptEncoding = UTF8.fromString "Accept-Encoding"
fromHeader HttpAcceptLanguage = UTF8.fromString "Accept-Language"
fromHeader HttpAuthorization = UTF8.fromString "Authorization"
fromHeader HttpExpect = UTF8.fromString "Expect"
fromHeader HttpFrom = UTF8.fromString "From"
fromHeader HttpHost = UTF8.fromString "Host"
fromHeader HttpIfMatch = UTF8.fromString "If-Match"
fromHeader HttpIfModifiedSince = UTF8.fromString "If-Modified-Since"
fromHeader HttpIfNoneMatch = UTF8.fromString "If-None-Match"
fromHeader HttpIfRange = UTF8.fromString "If-Range"
fromHeader HttpIfUnmodifiedSince = UTF8.fromString "If-Unmodified-Since"
fromHeader HttpMaxForwards = UTF8.fromString "Max-Forwards"
fromHeader HttpProxyAuthorization = UTF8.fromString "Proxy-Authorization"
fromHeader HttpRange = UTF8.fromString "Range"
fromHeader HttpReferrer = UTF8.fromString "Referer"
fromHeader HttpTE = UTF8.fromString "TE"
fromHeader HttpUserAgent = UTF8.fromString "User-Agent"
fromHeader HttpAcceptRanges = UTF8.fromString "Accept-Ranges"
fromHeader HttpAge = UTF8.fromString "Age"
fromHeader HttpETag = UTF8.fromString "ETag"
fromHeader HttpLocation = UTF8.fromString "Location"
fromHeader HttpProxyAuthenticate = UTF8.fromString "Proxy-Authenticate"
fromHeader HttpRetryAfter = UTF8.fromString "Retry-After"
fromHeader HttpServer = UTF8.fromString "Server"
fromHeader HttpVary = UTF8.fromString "Vary"
fromHeader HttpWWWAuthenticate = UTF8.fromString "WWW-Authenticate"
fromHeader HttpAllow = UTF8.fromString "Allow"
fromHeader HttpContentEncoding = UTF8.fromString "Content-Encoding"
fromHeader HttpContentLanguage = UTF8.fromString "Content-Language"
fromHeader HttpContentLength = UTF8.fromString "Content-Length"
fromHeader HttpContentLocation = UTF8.fromString "Content-Location"
fromHeader HttpContentMD5 = UTF8.fromString "Content-MD5"
fromHeader HttpContentRange = UTF8.fromString "Content-Range"
fromHeader HttpContentType = UTF8.fromString "Content-Type"
fromHeader HttpExpires = UTF8.fromString "Expires"
fromHeader HttpLastModified = UTF8.fromString "Last-Modified"
fromHeader (HttpExtensionHeader name) = name
fromHeader HttpCacheControl = UTF8.fromString "Cache-Control"
fromHeader HttpConnection = UTF8.fromString "Connection"
fromHeader HttpDate = UTF8.fromString "Date"
fromHeader HttpPragma = UTF8.fromString "Pragma"
fromHeader HttpTrailer = UTF8.fromString "Trailer"
fromHeader HttpTransferEncoding = UTF8.fromString "Transfer-Encoding"
fromHeader HttpUpgrade = UTF8.fromString "Upgrade"
fromHeader HttpVia = UTF8.fromString "Via"
fromHeader HttpWarning = UTF8.fromString "Warning"
fromHeader HttpCookie = UTF8.fromString "Cookie"
fromHeader HttpSetCookie = UTF8.fromString "Set-Cookie"


toHeader :: ByteString -> Header
toHeader bytestring
  | bytestring == UTF8.fromString "Accept" = HttpAccept
  | bytestring == UTF8.fromString "Accept-Charset" = HttpAcceptCharset
  | bytestring == UTF8.fromString "Accept-Encoding" = HttpAcceptEncoding
  | bytestring == UTF8.fromString "Accept-Language" = HttpAcceptLanguage
  | bytestring == UTF8.fromString "Authorization" = HttpAuthorization
  | bytestring == UTF8.fromString "Expect" = HttpExpect
  | bytestring == UTF8.fromString "From" = HttpFrom
  | bytestring == UTF8.fromString "Host" = HttpHost
  | bytestring == UTF8.fromString "If-Match" = HttpIfMatch
  | bytestring == UTF8.fromString "If-Modified-Since" = HttpIfModifiedSince
  | bytestring == UTF8.fromString "If-None-Match" = HttpIfNoneMatch
  | bytestring == UTF8.fromString "If-Range" = HttpIfRange
  | bytestring == UTF8.fromString "If-Unmodified-Since" = HttpIfUnmodifiedSince
  | bytestring == UTF8.fromString "Max-Forwards" = HttpMaxForwards
  | bytestring == UTF8.fromString "Proxy-Authorization" = HttpProxyAuthorization
  | bytestring == UTF8.fromString "Range" = HttpRange
  | bytestring == UTF8.fromString "Referer" = HttpReferrer
  | bytestring == UTF8.fromString "TE" = HttpTE
  | bytestring == UTF8.fromString "User-Agent" = HttpUserAgent
  | bytestring == UTF8.fromString "Accept-Ranges" = HttpAcceptRanges
  | bytestring == UTF8.fromString "Age" = HttpAge
  | bytestring == UTF8.fromString "ETag" = HttpETag
  | bytestring == UTF8.fromString "Location" = HttpLocation
  | bytestring == UTF8.fromString "Proxy-Authenticate" = HttpProxyAuthenticate
  | bytestring == UTF8.fromString "Retry-After" = HttpRetryAfter
  | bytestring == UTF8.fromString "Server" = HttpServer
  | bytestring == UTF8.fromString "Vary" = HttpVary
  | bytestring == UTF8.fromString "WWW-Authenticate" = HttpWWWAuthenticate
  | bytestring == UTF8.fromString "Allow" = HttpAllow
  | bytestring == UTF8.fromString "Content-Encoding" = HttpContentEncoding
  | bytestring == UTF8.fromString "Content-Language" = HttpContentLanguage
  | bytestring == UTF8.fromString "Content-Length" = HttpContentLength
  | bytestring == UTF8.fromString "Content-Location" = HttpContentLocation
  | bytestring == UTF8.fromString "Content-MD5" = HttpContentMD5
  | bytestring == UTF8.fromString "Content-Range" = HttpContentRange
  | bytestring == UTF8.fromString "Content-Type" = HttpContentType
  | bytestring == UTF8.fromString "Expires" = HttpExpires
  | bytestring == UTF8.fromString "Last-Modified" = HttpLastModified
  | bytestring == UTF8.fromString "Cache-Control" = HttpCacheControl
  | bytestring == UTF8.fromString "Connection" = HttpConnection
  | bytestring == UTF8.fromString "Date" = HttpDate
  | bytestring == UTF8.fromString "Pragma" = HttpPragma
  | bytestring == UTF8.fromString "Trailer" = HttpTrailer
  | bytestring == UTF8.fromString "Transfer-Encoding" = HttpTransferEncoding
  | bytestring == UTF8.fromString "Upgrade" = HttpUpgrade
  | bytestring == UTF8.fromString "Via" = HttpVia
  | bytestring == UTF8.fromString "Warning" = HttpWarning
  | bytestring == UTF8.fromString "Cookie" = HttpCookie
  | bytestring == UTF8.fromString "Set-Cookie" = HttpSetCookie
  | otherwise = HttpExtensionHeader bytestring


requestVariableNameIsHeader :: String -> Bool
requestVariableNameIsHeader name = (length name > 5) && (take 5 name == "HTTP_")


requestVariableNameToHeaderName :: String -> Maybe ByteString
requestVariableNameToHeaderName name
    = if requestVariableNameIsHeader name
        then let split [] = []
                 split string = case elemIndex '_' string of
                                  Nothing -> [string]
                                  Just index ->
                                    let (first, rest) = splitAt index string
                                    in first : (split $ drop 1 rest)
                 titleCase word = [toUpper $ head word] ++ (map toLower $ tail word)
                 headerName = intercalate "-" $ map titleCase $ split $ drop 5 name
             in Just $ UTF8.fromString headerName
        else Nothing


requestVariableNameToHeader :: String -> Maybe Header
requestVariableNameToHeader "HTTP_ACCEPT" = Just HttpAccept
requestVariableNameToHeader "HTTP_ACCEPT_CHARSET" = Just HttpAcceptCharset
requestVariableNameToHeader "HTTP_ACCEPT_ENCODING" = Just HttpAcceptEncoding
requestVariableNameToHeader "HTTP_ACCEPT_LANGUAGE" = Just HttpAcceptLanguage
requestVariableNameToHeader "HTTP_AUTHORIZATION" = Just HttpAuthorization
requestVariableNameToHeader "HTTP_EXPECT" = Just HttpExpect
requestVariableNameToHeader "HTTP_FROM" = Just HttpFrom
requestVariableNameToHeader "HTTP_HOST" = Just HttpHost
requestVariableNameToHeader "HTTP_IF_MATCH" = Just HttpIfMatch
requestVariableNameToHeader "HTTP_IF_MODIFIED_SINCE" = Just HttpIfModifiedSince
requestVariableNameToHeader "HTTP_IF_NONE_MATCH" = Just HttpIfNoneMatch
requestVariableNameToHeader "HTTP_IF_RANGE" = Just HttpIfRange
requestVariableNameToHeader "HTTP_IF_UNMODIFIED_SINCE" = Just HttpIfUnmodifiedSince
requestVariableNameToHeader "HTTP_MAX_FORWARDS" = Just HttpMaxForwards
requestVariableNameToHeader "HTTP_PROXY_AUTHORIZATION" = Just HttpProxyAuthorization
requestVariableNameToHeader "HTTP_RANGE" = Just HttpRange
requestVariableNameToHeader "HTTP_REFERER" = Just HttpReferrer
requestVariableNameToHeader "HTTP_TE" = Just HttpTE
requestVariableNameToHeader "HTTP_USER_AGENT" = Just HttpUserAgent
requestVariableNameToHeader "HTTP_ALLOW" = Just HttpAllow
requestVariableNameToHeader "HTTP_CONTENT_ENCODING" = Just HttpContentEncoding
requestVariableNameToHeader "HTTP_CONTENT_LANGUAGE" = Just HttpContentLanguage
requestVariableNameToHeader "HTTP_CONTENT_LENGTH" = Just HttpContentLength
requestVariableNameToHeader "HTTP_CONTENT_LOCATION" = Just HttpContentLocation
requestVariableNameToHeader "HTTP_CONTENT_MD5" = Just HttpContentMD5
requestVariableNameToHeader "HTTP_CONTENT_RANGE" = Just HttpContentRange
requestVariableNameToHeader "HTTP_CONTENT_TYPE" = Just HttpContentType
requestVariableNameToHeader "HTTP_EXPIRES" = Just HttpExpires
requestVariableNameToHeader "HTTP_LAST_MODIFIED" = Just HttpLastModified
requestVariableNameToHeader "HTTP_CACHE_CONTROL" = Just HttpCacheControl
requestVariableNameToHeader "HTTP_CONNECTION" = Just HttpConnection
requestVariableNameToHeader "HTTP_DATE" = Just HttpDate
requestVariableNameToHeader "HTTP_PRAGMA" = Just HttpPragma
requestVariableNameToHeader "HTTP_TRAILER" = Just HttpTrailer
requestVariableNameToHeader "HTTP_TRANSFER_ENCODING" = Just HttpTransferEncoding
requestVariableNameToHeader "HTTP_UPGRADE" = Just HttpUpgrade
requestVariableNameToHeader "HTTP_VIA" = Just HttpVia
requestVariableNameToHeader "HTTP_WARNING" = Just HttpWarning
requestVariableNameToHeader "HTTP_COOKIE" = Just HttpCookie
requestVariableNameToHeader name
    = if requestVariableNameIsHeader name
        then Just $ HttpExtensionHeader $ fromJust $ requestVariableNameToHeaderName name
        else Nothing


isValidInRequest :: Header -> Bool
isValidInRequest header = (headerType header == RequestHeader)
                          || (headerType header == EntityHeader)
                          || (headerType header == GeneralHeader)


isValidInResponse :: Header -> Bool
isValidInResponse header = (headerType header == ResponseHeader)
                           || (headerType header == EntityHeader)
                           || (headerType header == GeneralHeader)


isValidOnlyWithEntity :: Header -> Bool
isValidOnlyWithEntity header = headerType header == EntityHeader


-- | Queries the value of the CGI/1.1 request variable with the
--   given name for this request.  This interface is provided as a convenience to
--   programs which were originally written against the CGI or FastCGI APIs.
getRequestVariable
    :: (MonadHTTP m)
    => String -- ^ The name of the request variable to query.
    -> m (Maybe String) -- ^ The value of the request variable.
getRequestVariable "DOCUMENT_ROOT" = getDocumentRoot
getRequestVariable "GATEWAY_INTERFACE" = getGatewayInterface
getRequestVariable "PATH_INFO" = getPathInfo
getRequestVariable "PATH_TRANSLATED" = getPathTranslated
getRequestVariable "QUERY_STRING" = getQueryString
getRequestVariable "REDIRECT_STATUS" = do
  maybeResult <- getRedirectStatus
  case maybeResult of
    Nothing -> return Nothing
    Just result -> return $ Just $ show result
getRequestVariable "REDIRECT_URI" = getRedirectURI
getRequestVariable "REMOTE_ADDR" = do
  maybeResult <- getRemoteAddress
  case maybeResult of
    Nothing -> return Nothing
    Just result -> do
      result <- liftIO $ Network.inet_ntoa result
      return $ Just result
getRequestVariable "REMOTE_PORT" = do
  maybeResult <- getRemotePort
  case maybeResult of
    Nothing -> return Nothing
    Just result -> return $ Just $ show result
getRequestVariable "REMOTE_HOST" = getRemoteHost
getRequestVariable "REMOTE_IDENT" = getRemoteIdent
getRequestVariable "REMOTE_USER" = getRemoteUser
getRequestVariable "REQUEST_METHOD" = getRequestMethod
getRequestVariable "REQUEST_URI" = getRequestURI
getRequestVariable "SCRIPT_FILENAME" = getScriptFilename
getRequestVariable "SCRIPT_NAME" = getScriptName
getRequestVariable "SERVER_ADDR" = do
  maybeResult <- getServerAddress
  case maybeResult of
    Nothing -> return Nothing
    Just result -> do
      result <- liftIO $ Network.inet_ntoa result
      return $ Just result
getRequestVariable "SERVER_NAME" = getServerName
getRequestVariable "SERVER_PORT" = do
  maybeResult <- getServerPort
  case maybeResult of
    Nothing -> return Nothing
    Just result -> return $ Just $ show result
getRequestVariable "SERVER_PROTOCOL" = getServerProtocol
getRequestVariable "SERVER_SOFTWARE" = getServerSoftware
getRequestVariable "AUTH_TYPE" = getAuthenticationType
getRequestVariable "CONTENT_LENGTH" = do
  maybeResult <- getContentLength
  case maybeResult of
    Nothing -> return Nothing
    Just result -> return $ Just $ show result
getRequestVariable "CONTENT_TYPE" = getContentType
getRequestVariable string
  | requestVariableNameIsHeader string
    = getRequestHeader $ fromJust $ requestVariableNameToHeader string
  | otherwise = return Nothing


-- | Returns an association list of name-value pairs of all the CGI/1.1 request
--   variables from the web server.  This interface is provided as a convenience
--   to programs which were originally written against the CGI or FastCGI APIs.
--   Its use is not recommended if only some values are needed, because it
--   implicitly calls getRemoteHost and getRemoteIdent, which are potentially
--   time-consuming.
getAllRequestVariables
    :: (MonadHTTP m) => m [(String, String)]
getAllRequestVariables = do
  result
    <- mapM (\name -> do
               maybeValue <- getRequestVariable name
               case maybeValue of
                 Nothing -> return Nothing
                 Just value -> return $ Just (name, value))
            ["DOCUMENT_ROOT", "GATEWAY_INTERFACE", "PATH_INFO",
             "PATH_TRANSLATED", "QUERY_STRING", "REDIRECT_STATUS",
             "REDIRECT_URI", "REMOTE_ADDR", "REMOTE_PORT", "REMOTE_HOST",
             "REMOTE_IDENT", "REMOTE_USER", "REQUEST_METHOD", "REQUEST_URI",
             "SCRIPT_FILENAME", "SCRIPT_NAME", "SERVER_ADDR", "SERVER_NAME",
             "SERVER_PORT", "SERVER_PROTOCOL", "SERVER_SOFTWARE", "AUTH_TYPE",
             "CONTENT_LENGTH", "CONTENT_TYPE", "HTTP_ACCEPT",
             "HTTP_ACCEPT_CHARSET", "HTTP_ACCEPT_ENCODING",
             "HTTP_ACCEPT_LANGUAGE", "HTTP_AUTHORIZATION", "HTTP_EXPECT",
             "HTTP_FROM", "HTTP_HOST", "HTTP_IF_MATCH",
             "HTTP_IF_MODIFIED_SINCE", "HTTP_IF_NONE_MATCH", "HTTP_IF_RANGE",
             "HTTP_IF_UNMODIFIED_SINCE", "HTTP_MAX_FORWARDS",
             "HTTP_PROXY_AUTHORIZATION", "HTTP_RANGE", "HTTP_REFERER",
             "HTTP_TE", "HTTP_USER_AGENT", "HTTP_ALLOW",
             "HTTP_CONTENT_ENCODING", "HTTP_CONTENT_LANGUAGE",
             "HTTP_CONTENT_LENGTH", "HTTP_CONTENT_LOCATION",
             "HTTP_CONTENT_MD5", "HTTP_CONTENT_RANGE", "HTTP_CONTENT_TYPE",
             "HTTP_EXPIRES", "HTTP_LAST_MODIFIED", "HTTP_CACHE_CONTROL",
             "HTTP_CONNECTION", "HTTP_DATE", "HTTP_PRAGMA", "HTTP_TRAILER",
             "HTTP_TRANSFER_ENCODING", "HTTP_UPGRADE", "HTTP_VIA",
             "HTTP_WARNING", "HTTP_COOKIE"]
  return $ map fromJust $ filter isJust result


-- | Queries the value from the user agent of the given HTTP/1.1 header.  If the
--   header is to be provided after the content as specified by the Trailer
--   header, this is potentially time-consuming.
getRequestHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to query.  Must be a request or entity header.
    -> m (Maybe String) -- ^ The value of the header, if the user agent provided one.
getRequestHeader header = do
  HTTPConnection { httpConnectionRequestHeaderMap = mvar } <- getHTTPConnection
  headerMap <- liftIO $ readMVar mvar
  return $ fmap (stripHeaderValueWhitespace . UTF8.toString)
                $ Map.lookup header headerMap


stripHeaderValueWhitespace :: String -> String
stripHeaderValueWhitespace input =
  let input' = reverse $ dropWhile isHeaderValueWhitespace
                       $ reverse $ dropWhile isHeaderValueWhitespace input
      computeWords input = case break isHeaderValueWhitespace input of
                             (all, "") -> [all]
                             (before, after)
                               -> [before]
                                  ++ (computeWords
                                      $ dropWhile isHeaderValueWhitespace after)
      words = computeWords input'
      output = intercalate " " words
  in output


isHeaderValueWhitespace :: Char -> Bool
isHeaderValueWhitespace char = elem char " \t\r\n"


-- | Returns an association list of name-value pairs of all the HTTP/1.1 request
--   or entity headers from the user agent.  If some of these headers are to be
--   provided after the content as specified by the Trailer header, this is
--   potentially time-consuming.
getAllRequestHeaders :: (MonadHTTP m) => m [(Header, String)]
getAllRequestHeaders = do
  HTTPConnection { httpConnectionRequestHeaderMap = mvar } <- getHTTPConnection
  headerMap <- liftIO $ readMVar mvar
  return $ map (\(header, bytestring) -> (header, UTF8.toString bytestring))
               $ Map.toList headerMap


-- | Returns a 'Cookie' object for the given name, if the user agent provided one
--   in accordance with RFC 2109.
getCookie
    :: (MonadHTTP m)
    => String -- ^ The name of the cookie to look for.
    -> m (Maybe Cookie) -- ^ The cookie, if the user agent provided it.
getCookie name = do
  return Nothing
  -- TODO


-- | Returns all 'Cookie' objects provided by the user agent in accordance 
--   RFC 2109.
getAllCookies :: (MonadHTTP m) => m [Cookie]
getAllCookies = do
  return []
  -- TODO


-- | A convenience method; as 'getCookie', but returns only the value of the cookie
--   rather than a 'Cookie' object.
getCookieValue
    :: (MonadHTTP m)
    => String -- ^ The name of the cookie to look for.
    -> m (Maybe String) -- ^ The value of the cookie, if the user agent provided it.
getCookieValue name = do
  return Nothing
  -- TODO


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the document root.
getDocumentRoot :: (MonadHTTP m) => m (Maybe String)
getDocumentRoot = do
  return Nothing


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the gateway interface.
getGatewayInterface :: (MonadHTTP m) => m (Maybe String)
getGatewayInterface = do
  return Nothing


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the path info.
getPathInfo :: (MonadHTTP m) => m (Maybe String)
getPathInfo = do
  return Nothing


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the path-translated value.
getPathTranslated :: (MonadHTTP m) => m (Maybe String)
getPathTranslated = do
  return Nothing


-- | Return the query string, as provided by the user agent.
getQueryString :: (MonadHTTP m) => m (Maybe String)
getQueryString = do
  HTTPConnection { httpConnectionQueryString = mvar } <- getHTTPConnection
  liftIO $ readMVar mvar


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the redirect status.
getRedirectStatus :: (MonadHTTP m) => m (Maybe Int)
getRedirectStatus = do
  return Nothing


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the redirect URI.
getRedirectURI :: (MonadHTTP m) => m (Maybe String)
getRedirectURI = do
  return Nothing


-- | Return the remote address.
getRemoteAddress :: (MonadHTTP m) => m (Maybe Network.HostAddress)
getRemoteAddress = do
  HTTPConnection { httpConnectionPeer = peer } <- getHTTPConnection
  case peer of
    Network.SockAddrInet _ address -> return $ Just address


-- | Return the remote port.
getRemotePort :: (MonadHTTP m) => m (Maybe Int)
getRemotePort = do
  HTTPConnection { httpConnectionPeer = peer } <- getHTTPConnection
  case peer of
    Network.SockAddrInet port _ -> do
      return $ Just $ fromIntegral port


-- | Return the remote hostname, as determined by the web server.  If it has
--   not yet been looked up, performs the lookup.  This is potentially
--   time-consuming.
getRemoteHost :: (MonadHTTP m) => m (Maybe String)
getRemoteHost = do
  HTTPConnection { httpConnectionRemoteHostname = mvar } <- getHTTPConnection
  maybeMaybeHostname <- liftIO $ readMVar mvar
  case maybeMaybeHostname of
    Nothing -> do
      HTTPConnection { httpConnectionPeer = peer } <- getHTTPConnection
      inputHostname <- case peer of
        Network.SockAddrInet _ address -> do
          string <- liftIO $ Network.inet_ntoa address
          return string
      maybeValues <- liftIO $ Network.getAddrInfo
                               (Just $ Network.defaultHints {
                                         Network.addrFlags
                                           = [Network.AI_CANONNAME]
                                       })
                               (Just inputHostname)
                               Nothing
      let maybeHostname = case maybeValues of
                            (Network.AddrInfo {
                                        Network.addrCanonName = Just result
                                      }:_)
                              -> Just result
                            _ -> Nothing
      liftIO $ swapMVar mvar $ Just maybeHostname
      return maybeHostname
    Just maybeHostname -> return maybeHostname


-- | Return the remote ident value, as determined by the web server.  If it has
--   not yet been looked up, performs the lookup.  This is potentially
--   time-consuming.  Not yet implemented; currently, always returns Nothing.
getRemoteIdent :: (MonadHTTP m) => m (Maybe String)
getRemoteIdent = do
  return Nothing


-- | Return the remote user name.  Not yet implemented; currently, always
--   returns Nothing.
getRemoteUser :: (MonadHTTP m) => m (Maybe String)
getRemoteUser = do
  return Nothing
  -- TODO


-- | Return the request method.
getRequestMethod :: (MonadHTTP m) => m (Maybe String)
getRequestMethod = do
  HTTPConnection { httpConnectionRequestMethod = mvar } <- getHTTPConnection
  liftIO $ readMVar mvar >>= return . Just


-- | Return the request URI.
getRequestURI :: (MonadHTTP m) => m (Maybe String)
getRequestURI = do
  HTTPConnection { httpConnectionRequestURI = mvar } <- getHTTPConnection
  liftIO $ readMVar mvar >>= return . Just


getRequestProtocol :: (MonadHTTP m) => m (Maybe String)
getRequestProtocol = do
  HTTPConnection { httpConnectionRequestProtocol = protocolMVar } <- getHTTPConnection
  liftIO $ readMVar protocolMVar >>= return . Just


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the script filename.
getScriptFilename :: (MonadHTTP m) => m (Maybe String)
getScriptFilename = do
  return Nothing


-- | Return Nothing.  Provided for compatibility with direct-fastcgi, in which
--   the corresponding function would return the script name.
getScriptName :: (MonadHTTP m) => m (Maybe String)
getScriptName = do
  return Nothing


-- | Return the server address.
getServerAddress :: (MonadHTTP m) => m (Maybe Network.HostAddress)
getServerAddress = do
  return Nothing
  -- TODO


-- | Return the server name.
getServerName :: (MonadHTTP m) => m (Maybe String)
getServerName = do
  return Nothing
  -- TODO


-- | Return the server port.
getServerPort :: (MonadHTTP m) => m (Maybe Int)
getServerPort = do
  return Nothing
  -- TODO


-- | Return the server protocol.
getServerProtocol :: (MonadHTTP m) => m (Maybe String)
getServerProtocol = do
  return Nothing
  -- TODO


-- | Return the server software name and version.
getServerSoftware :: (MonadHTTP m) => m (Maybe String)
getServerSoftware = do
  return $ Just "direct-http 1.0"


-- | Return the authentication type.
getAuthenticationType :: (MonadHTTP m) => m (Maybe String)
getAuthenticationType = do
  return Nothing
  -- TODO


-- | Return the request content length, if this is knowable without actually
--   receiving the content - in particular, if the Content-Length header was
--   used.  Otherwise, returns Nothing.
getContentLength :: (MonadHTTP m) => m (Maybe Int)
getContentLength = do
  maybeString <- getRequestHeader HttpContentLength
  case maybeString of
    Nothing -> return Nothing
    Just string -> return $ parseInt string


-- | Return the request content type, as provided by the user agent.
getContentType :: (MonadHTTP m) => m (Maybe String)
getContentType = do
  getRequestHeader HttpContentType


getRequestHasContent :: (MonadHTTP m) => m Bool
getRequestHasContent = do
  HTTPConnection { httpConnectionRequestContentParameters = parametersMVar }
    <- getHTTPConnection
  parameters <- liftIO $ takeMVar parametersMVar
  parameters <- ensureRequestContentParametersInitialized parameters
  liftIO $ putMVar parametersMVar parameters
  return $ case parameters of
    RequestContentNone -> False
    _ -> True


getRequestContentAllowed :: (MonadHTTP m) => m Bool
getRequestContentAllowed = do
  Just method <- getRequestMethod
  case method of
    _ | method == "OPTIONS" -> return True
      | method == "GET" -> return False
      | method == "HEAD" -> return False
      | method == "POST" -> return True
      | method == "PUT" -> return True
      | method == "DELETE" -> return False
      | method == "TRACE" -> return False
      | method == "CONNECT" -> return True
      | otherwise -> return True


-- | Reads up to a specified amount of data from the content of the HTTP
--   request, if any, and interprets it as binary data.  If input has been
--   closed, returns an empty bytestring.  If no input is immediately
--   available, blocks until there is some.  If output has been closed, causes
--   an 'OutputAlreadyClosed' exception.
httpGet :: (MonadHTTP m) => Int -> m BS.ByteString
httpGet size = httpGet' (Just size) True False


-- | Reads up to a specified amount of data from the content of the HTTP
--   request, if any, and interprets it as binary data.  If input has been
--   closed, returns an empty bytestring.  If insufficient input is available,
--   returns any input which is immediately available, or an empty bytestring
--   if there is none, never blocking.  If output has been closed, causes an
--   'OutputAlreadyClosed' exception.
httpGetNonBlocking :: (MonadHTTP m) => Int -> m BS.ByteString
httpGetNonBlocking size = httpGet' (Just size) False False


-- | Reads all remaining data from the content of the HTTP request, if any,
--   and interprets it as binary data.  Blocks until all input has been
--   read.  If input has been closed, returns an empty bytestring.  If output
--   has been closed, causes an 'OutputAlreadyClosed' exception.
httpGetContents :: (MonadHTTP m) => m BS.ByteString
httpGetContents = httpGet' Nothing True False


-- | Returns whether the content of the HTTP request potentially has data
--   remaining, either in the buffer or yet to be read.
httpIsReadable :: (MonadHTTP m) => m Bool
httpIsReadable = do
  HTTPConnection { httpConnectionRequestContentParameters = parametersMVar }
    <- getHTTPConnection
  parameters <- liftIO $ takeMVar parametersMVar
  parameters <- ensureRequestContentParametersInitialized parameters
  liftIO $ putMVar parametersMVar parameters
  return $ case parameters of
             RequestContentNone -> False
             RequestContentClosed -> False
             _ -> True


httpGet' :: (MonadHTTP m) => (Maybe Int) -> Bool -> Bool -> m BS.ByteString
httpGet' maybeSize blocking discarding = do
  if not discarding
    then requireOutputNotYetClosed
    else return ()
  HTTPConnection {
      httpConnectionRequestContentBuffer = bufferMVar,
      httpConnectionRequestContentParameters = parametersMVar
    } <- getHTTPConnection
  buffer <- liftIO $ takeMVar bufferMVar
  parameters <- liftIO $ takeMVar parametersMVar
  parameters <- ensureRequestContentParametersInitialized parameters
  (buffer, parameters)
    <- extendRequestContentBuffer buffer parameters maybeSize blocking
  (result, buffer) <- return $ case maybeSize of
                        Nothing -> (buffer, BS.empty)
                        Just size -> BS.splitAt size buffer
  liftIO $ putMVar parametersMVar parameters
  liftIO $ putMVar bufferMVar buffer
  return result


ensureRequestContentParametersInitialized
  :: (MonadHTTP m)
  => RequestContentParameters
  -> m RequestContentParameters
ensureRequestContentParametersInitialized RequestContentUninitialized = do
  maybeLength <- getContentLength
  maybeTransferEncodingString <- getRequestHeader HttpTransferEncoding
  let (hasContent, chunked)
        = case (maybeLength, maybeTransferEncodingString) of
            (Nothing, Nothing) -> (False, False)
            (Just length, Nothing) -> (True, False)
            (Just length, Just encoding)
              | map toLower encoding == "identity" -> (True, False)
              | otherwise -> (True, True)
            (_, Just _) -> (True, True)
  if hasContent
    then if chunked
           then return $ RequestContentChunked False 0
           else return $ RequestContentIdentity $ fromJust maybeLength
    else return RequestContentNone
ensureRequestContentParametersInitialized parameters = return parameters


extendRequestContentBuffer
  :: (MonadHTTP m)
  => BS.ByteString
  -> RequestContentParameters
  -> (Maybe Int)
  -> Bool
  -> m (BS.ByteString, RequestContentParameters)
extendRequestContentBuffer highLevelBuffer
                           parameters
                           maybeTargetLength
                           blocking = do
  let isAtLeastTargetLength buffer =
        case maybeTargetLength of
          Nothing -> False
          Just targetLength -> BS.length buffer >= targetLength
      loop highLevelBuffer lowLevelBuffer parameters = do
        if isAtLeastTargetLength highLevelBuffer
          then return (highLevelBuffer, lowLevelBuffer, parameters)
          else do
            case parameters of
              RequestContentNone
                -> return (highLevelBuffer, lowLevelBuffer, parameters)
              RequestContentClosed
                -> return (highLevelBuffer, lowLevelBuffer, parameters)
              RequestContentIdentity lengthRemaining -> do
                (lowLevelBuffer, endOfInput)
                  <- extendInputBuffer lowLevelBuffer lengthRemaining blocking
                if endOfInput
                  then httpThrow UnexpectedEndOfInput
                  else return ()
                let (toHighLevelBuffer, lowLevelBuffer')
                      = BS.splitAt lengthRemaining lowLevelBuffer
                    lengthRead = BS.length toHighLevelBuffer
                    highLevelBuffer'
                      = BS.append highLevelBuffer toHighLevelBuffer
                    lengthRemaining' = if lengthRemaining > lengthRead
                                         then lengthRemaining - lengthRead
                                         else 0
                    parameters' = if lengthRemaining' > 0
                                    then RequestContentIdentity lengthRemaining'
                                    else RequestContentClosed
                if not blocking || isAtLeastTargetLength highLevelBuffer
                  then return (highLevelBuffer', lowLevelBuffer', parameters')
                  else loop highLevelBuffer' lowLevelBuffer' parameters'
              RequestContentChunked _ _ -> do
                 httpLog $ "Don't understand chunked."
                 httpThrow UnexpectedEndOfInput
                 -- TODO
  HTTPConnection { httpConnectionInputBufferMVar = lowLevelBufferMVar }
    <- getHTTPConnection
  lowLevelBuffer <- liftIO $ takeMVar lowLevelBufferMVar
  (highLevelBuffer, lowLevelBuffer, parameters)
    <- loop highLevelBuffer lowLevelBuffer parameters
  liftIO $ putMVar lowLevelBufferMVar lowLevelBuffer
  return (highLevelBuffer, parameters)


-- | Sets the response status which will be sent with the response headers.  If
--   the response headers have already been sent, or are no longer modifiable
--   (because of a call to 'httpPut' or similar), causes a
--   'ResponseHeadersAlreadySent' or 'ResponseHeadersNotModifiable' exception.
setResponseStatus
    :: (MonadHTTP m)
    => Int -- ^ The HTTP/1.1 status code to set.
    -> m ()
setResponseStatus status = do
  requireResponseHeadersNotYetSent
  requireResponseHeadersModifiable
  HTTPConnection { httpConnectionResponseStatus = mvar } <- getHTTPConnection
  liftIO $ swapMVar mvar status
  return ()
      


-- | Returns the response status which will be or has been sent with the response
--   headers.
getResponseStatus
    :: (MonadHTTP m)
    => m Int -- ^ The HTTP/1.1 status code.
getResponseStatus = do
  HTTPConnection { httpConnectionResponseStatus = mvar } <- getHTTPConnection
  liftIO $ readMVar mvar


-- | Sets the given 'HttpHeader' response header to the given string value,
--   overriding any value which has previously been set.  If the response
--   headers have already been sent, or are no longer modifiable (because of a
--   call to 'httpPut' or similar), causes a 'ResponseHeadersAlreadySent'
--   or 'ResponseHeadersNotModifiable'
--   exception.  If the header is not an HTTP/1.1 or extension response,
--   entity, or general header, ie, is not valid as part of a response, causes
--   a 'NotAResponseHeader' exception.
--   
--   If a value is set for the 'HttpSetCookie' header, this overrides all
--   cookies set for this request with 'setCookie'.
setResponseHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to set.  Must be a response header or an entity header.
    -> String -- ^ The value to set.
    -> m ()
setResponseHeader header value = do
  requireResponseHeadersNotYetSent
  requireResponseHeadersModifiable
  if isValidInResponse header
    then do
      HTTPConnection { httpConnectionResponseHeaderMap = mvar } <- getHTTPConnection
      headerMap <- liftIO $ takeMVar mvar
      headerMap <- return $ Map.insert header (UTF8.fromString value) headerMap
      liftIO $ putMVar mvar headerMap
    else httpThrow $ NotAResponseHeader header


-- | Causes the given 'HttpHeader' response header not to be sent, overriding
--   any value which has previously been set.  If the response headers have
--   already been sent, or are no longer modifiable (because of a call to
--   'httpPut' or similar), causes a 'ResponseHeadersAlreadySent' or
--   'ResponseHeadersNotModifiable' exception.  If
--   the header is not an HTTP/1.1 or extension response or entity header, ie,
--   is not valid as part of a response, causes a 'NotAResponseHeader'
--   exception.
--   
--   Does not prevent the 'HttpSetCookie' header from being sent if cookies
--   have been set for this request with 'setCookie'.
unsetResponseHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to unset.  Must be a response header or an entity header.
    -> m ()
unsetResponseHeader header = do
  requireResponseHeadersNotYetSent
  requireResponseHeadersModifiable
  if isValidInResponse header
    then do
      HTTPConnection { httpConnectionResponseHeaderMap = mvar } <- getHTTPConnection
      headerMap <- liftIO $ takeMVar mvar
      headerMap <- return $ Map.delete header headerMap
      liftIO $ putMVar mvar headerMap
    else httpThrow $ NotAResponseHeader header


-- | Returns the value of the given header which will be or has been sent with
--   the response headers.  If the header is not an HTTP/1.1 or extension
--   response, entity, or general header, ie, is not valid as part of a
--   response, causes a 'NotAResponseHeader' exception.
getResponseHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to query.  Must be a response header or an entity
              --   header.
    -> m (Maybe String) -- ^ The value of the queried header.
getResponseHeader header = do
  if isValidInResponse header
    then do
      HTTPConnection { httpConnectionResponseHeaderMap = mvar } <- getHTTPConnection
      headerMap <- liftIO $ readMVar mvar
      return $ fmap UTF8.toString $ Map.lookup header headerMap
    else httpThrow $ NotAResponseHeader header


-- | Causes the user agent to record the given cookie and send it back with
--   future loads of this page.  Does not take effect instantly, but rather
--   when headers are sent.  Cookies are set in accordance with RFC 2109.
--   If an @HttpCookie@ header is set for this request by a call to
--   'setResponseHeader', this function has no effect.  If the response headers
--   have already been sent, or are no longer modifiable (because of a call to
--   'httpPut' or similar), causes a 'ResponseHeadersAlreadySent' or
--   'ResponseHeadersNotModifiable' exception.
--   If the name is not a possible name for a cookie, causes a 'CookieNameInvalid'
--   exception.
setCookie
    :: (MonadHTTP m)
    => Cookie -- ^ The cookie to set.
    -> m ()
setCookie cookie = do
  requireResponseHeadersNotYetSent
  requireResponseHeadersModifiable
  requireValidCookieName $ cookieName cookie
  {-
  HTTPConnection { request = Just request } <- getHTTPConnection
  responseCookieMap <- liftIO $ takeMVar $ responseCookieMapMVar request
  let responseCookieMap' = Map.insert (cookieName cookie) cookie responseCookieMap
  liftIO $ putMVar (responseCookieMapMVar request) responseCookieMap'
  -}
  return ()
  -- TODO


-- | Causes the user agent to unset any cookie applicable to this page with the
--   given name.  Does not take effect instantly, but rather when headers are
--   sent.  If an @HttpCookie@ header is set for this request by a call to
--   'setResponseHeader', this function has no effect.  If the response headers
--   have already been sent, or are no longer modifiable (because of a call to
--   'httpPut' or similar), causes a 'ResponseHeadersAlreadySent' or
--   'ResponseHeadersNotModifiable' exception.
--   If the name is not a possible name for a cookie, causes a
--   'CookieNameInvalid' exception.
unsetCookie
    :: (MonadHTTP m)
    => String -- ^ The name of the cookie to unset.
    -> m ()
unsetCookie name = do
  requireResponseHeadersNotYetSent
  requireResponseHeadersModifiable
  requireValidCookieName name
  {-
  HTTPConnection { request = Just request } <- getHTTPConnection
  responseCookieMap <- liftIO $ takeMVar $ responseCookieMapMVar request
  let responseCookieMap' = Map.insert name (mkUnsetCookie name) responseCookieMap
  liftIO $ putMVar (responseCookieMapMVar request) responseCookieMap'
  -}
  return ()
  -- TODO


-- | Constructs a cookie with the given name and value.  Version is set to 1;
--   path, domain, and maximum age are set to @Nothing@; and the secure flag is
--   set to @False@.  Constructing the cookie does not cause it to be set; to do
--   that, call 'setCookie' on it.
mkSimpleCookie
    :: String -- ^ The name of the cookie to construct.
    -> String -- ^ The value of the cookie to construct.
    -> Cookie -- ^ A cookie with the given name and value.
mkSimpleCookie name value = Cookie {
                              cookieName = name,
                              cookieValue = value,
                              cookieVersion = 1,
                              cookiePath = Nothing,
                              cookieDomain = Nothing,
                              cookieMaxAge = Nothing,
                              cookieSecure = False,
                              cookieComment = Nothing
                            }


-- | Constructs a cookie with the given parameters.  Version is set to 1.
--   Constructing the cookie does not cause it to be set; to do that, call 'setCookie'
--   on it.
mkCookie
    :: String -- ^ The name of the cookie to construct.
    -> String -- ^ The value of the cookie to construct.
    -> (Maybe String) -- ^ The path of the cookie to construct.
    -> (Maybe String) -- ^ The domain of the cookie to construct.
    -> (Maybe Int) -- ^ The maximum age of the cookie to construct, in seconds.
    -> Bool -- ^ Whether to flag the cookie to construct as secure.
    -> Cookie -- ^ A cookie with the given parameters.
mkCookie name value maybePath maybeDomain maybeMaxAge secure
    = Cookie {
        cookieName = name,
        cookieValue = value,
        cookieVersion = 1,
        cookiePath = maybePath,
        cookieDomain = maybeDomain,
        cookieMaxAge = maybeMaxAge,
        cookieSecure = secure,
        cookieComment = Nothing
      }


mkUnsetCookie :: String -> Cookie
mkUnsetCookie name  = Cookie {
                              cookieName = name,
                              cookieValue = "",
                              cookieVersion = 1,
                              cookiePath = Nothing,
                              cookieDomain = Nothing,
                              cookieMaxAge = Just 0,
                              cookieSecure = False,
                              cookieComment = Nothing
                            }


requireValidCookieName :: (MonadHTTP m) => String -> m ()
requireValidCookieName name = do
  if not $ isValidCookieToken name
    then httpThrow $ CookieNameInvalid name
    else return ()


isValidCookieToken :: String -> Bool
isValidCookieToken token =
    let validCharacter c = (ord c > 0) && (ord c < 128)
                           && (not $ elem c "()<>@,;:\\\"/[]?={} \t")
    in (length token > 0) && (all validCharacter token)


-- | An exception originating within the HTTP infrastructure or the web server.
data HTTPException
    = ResponseHeadersAlreadySent
      -- ^ An exception thrown by operations which require the response headers not
      --   to have been sent yet.
    | ResponseHeadersNotModifiable
      -- ^ An exception thrown by operations which require the response headers
      --   to still be modifiable.
    | OutputAlreadyClosed
      -- ^ An exception thrown by operations which produce output when output has
      --   been closed, as by 'httpCloseOutput'.
    | OutputIncomplete
      -- ^ An exception thrown when output is closed, as by 'httpCloseOutput',
      --   when the response headers imply that there will be a certain amount
      --   of data and there is not.
    | NotAResponseHeader Header
      -- ^ An exception thrown by operations which are given a header that does not
      --   meet their requirement of being valid in a response.
    | CookieNameInvalid String
      -- ^ An exception thrown by operations which are given cookie names that do not
      --   meet the appropriate syntax requirements.
      deriving (Show, Typeable)


instance Exception.Exception HTTPException


-- | Sets the HTTP/1.1 return status to 301 and sets the 'HttpLocation' header
--   to the provided URL.  This has the effect of issuing a permanent redirect
--   to the user agent.  Permanent redirects, as opposed to temporary redirects,
--   may cause bookmarks or incoming links to be updated.  If the response
--   headers have already been sent, or are no longer modifiable (because of a
--   call to 'httpPut' or similar), causes a 'ResponseHeadersAlreadySent' or
--   'ResponseHeadersNotModifiable' exception.
permanentRedirect
    :: (MonadHTTP m)
    => String -- ^ The URL to redirect to, as a string.
    -> m ()
permanentRedirect url = do
  setResponseStatus 301
  setResponseHeader HttpLocation url


-- | Sets the HTTP/1.1 return status to 303 and sets the 'HttpLocation' header
--   to the provided URL.  This has the effect of issuing a see-other or
--   "temporary" redirect to the user agent.  Temporary redirects, as opposed to
--   permanent redirects, do not cause bookmarks or incoming links to be
--   updated.  If the response headers have already been sent, or are no longer
--   modifiable (because of a call to 'httpPut' or similar), causes a
--   'ResponseHeadersAlreadySent' or 'ResponseHeadersNotModifiable' exception.
seeOtherRedirect
    :: (MonadHTTP m)
    => String -- ^ The URL to redirect to, as a string.
    -> m ()
seeOtherRedirect url = do
  setResponseStatus 303
  setResponseHeader HttpLocation url


-- | Ensures that the response headers have been sent.  If they are already
--   sent, does nothing.  If output has already been closed, causes an
--   'OutputAlreadyClosed' exception.
sendResponseHeaders :: (MonadHTTP m) => m ()
sendResponseHeaders = do
  requireOutputNotYetClosed
  HTTPConnection {
      httpConnectionSocket = socket,
      httpConnectionResponseHeadersSent = alreadySentMVar,
      httpConnectionResponseHeadersModifiable = modifiableMVar,
      httpConnectionResponseStatus = responseStatusMVar,
      httpConnectionResponseHeaderMap = responseHeaderMapMVar,
      httpConnectionResponseCookieMap = responseCookieMapMVar
    } <- getHTTPConnection
  alreadySent <- liftIO $ takeMVar alreadySentMVar
  if not alreadySent
    then do
      liftIO $ swapMVar modifiableMVar False
      responseStatus <- liftIO $ readMVar responseStatusMVar
      responseHeaderMap <- liftIO $ readMVar responseHeaderMapMVar
      responseCookieMap <- liftIO $ readMVar responseCookieMapMVar
      let statusLine = BS.concat [UTF8.fromString "HTTP/1.1 ",
                                  UTF8.fromString $ show responseStatus,
                                  UTF8.fromString " ",
                                  reasonPhrase responseStatus,
                                  UTF8.fromString "\r\n"]
          nameValuePairs
            = concat [map (\(header, value) -> (fromHeader header, value))
                          $ Map.toList responseHeaderMap,
                      if (isNothing $ Map.lookup HttpSetCookie responseHeaderMap)
                         && (not $ Map.null responseCookieMap)
                        then [(UTF8.fromString "Set-Cookie", setCookieValue)]
                        else []]
          setCookieValue = printCookies $ Map.elems responseCookieMap
          delimiterLine = UTF8.fromString "\r\n"
          buffer = BS.concat $ [statusLine]
                               ++ (concat
                                   $ map (\(name, value)
                                            -> [name, UTF8.fromString ": ",
                                                value, UTF8.fromString "\r\n"])
                                         nameValuePairs)
                               ++ [delimiterLine]
      liftIO $ Network.sendAll socket buffer
    else return ()
  liftIO $ putMVar alreadySentMVar True


markResponseHeadersUnmodifiable :: (MonadHTTP m) => m ()
markResponseHeadersUnmodifiable = do
  HTTPConnection { httpConnectionResponseHeadersModifiable = modifiableMVar }
    <- getHTTPConnection
  liftIO $ swapMVar modifiableMVar False
  return ()


reasonPhrase :: Int -> ByteString
reasonPhrase status =
  UTF8.fromString $ case status of
                      100 -> "Continue"
                      101 -> "Switching Protocols"
                      200 -> "OK"
                      201 -> "Created"
                      202 -> "Accepted"
                      203 -> "Non-Authoritative Information"
                      204 -> "No Content"
                      205 -> "Reset Content"
                      206 -> "Partial Content"
                      300 -> "Multiple Choices"
                      301 -> "Moved Permanently"
                      302 -> "Found"
                      303 -> "See Other"
                      304 -> "Not Modified"
                      305 -> "Use Proxy"
                      307 -> "Temporary Redirect"
                      400 -> "Bad Request"
                      401 -> "Unauthorized"
                      402 -> "Payment Required"
                      403 -> "Forbidden"
                      404 -> "Not Found"
                      405 -> "Method Not Allowed"
                      406 -> "Not Acceptable"
                      407 -> "Proxy Authentication Required"
                      408 -> "Request Time-out"
                      409 -> "Conflict"
                      410 -> "Gone"
                      411 -> "Length Required"
                      412 -> "Precondition Failed"
                      413 -> "Request Entity Too Large"
                      414 -> "Request-URI Too Large"
                      415 -> "Unsupported Media Type"
                      416 -> "Requested range not satisfiable"
                      417 -> "Expectation Failed"
                      500 -> "Internal Server Error"
                      501 -> "Not Implemented"
                      502 -> "Bad Gateway"
                      503 -> "Service Unavailable"
                      504 -> "Gateway Time-out"
                      505 -> "HTTP Version not supported"
                      _ -> "Extension"


-- | Returns whether the response headers have been sent, regardless of whether
--   they are modifiable (they might not be because of a call to 'httpPut' or
--   similar).
responseHeadersSent :: (MonadHTTP m) => m Bool
responseHeadersSent = do
  HTTPConnection { httpConnectionResponseHeadersSent = mvar }
    <- getHTTPConnection
  liftIO $ readMVar mvar


-- | Returns whether the response headers are modifiable, a prerequisite of
--   which is that they have not already been sent.  (They might not be
--   modifiable because of a call to 'httpPut' or similar.)
responseHeadersModifiable :: (MonadHTTP m) => m Bool
responseHeadersModifiable = do
  HTTPConnection { httpConnectionResponseHeadersModifiable = mvar }
    <- getHTTPConnection
  liftIO $ readMVar mvar


-- | Appends data, interpreted as binary, to the content of the HTTP response.
--   Makes the response headers no longer modifiable, effective immediately.
--   If output has already been closed, causes an 'OutputAlreadyClosed'
--   exception.  If the response Transfer-Encoding as set in the response
--   headers is "identity" or omitted, and the response Content-Length is
--   omitted, data is buffered until output is closed, then sent all at once
--   with an appropriate Content-Length header.  Otherwise - that is, if there
--   is a Transfer-Encoding other than "identity" set, or if Content-Length is
--   set - data is sent immediately.  If Content-Length is set, and the
--   provided data would cause the cumulative data sent to exceed that length,
--   causes an 'OutputAlreadyClosed' exception.  At the time that data is
--   actually sent, if the response headers have not been sent, first sends
--   them.
--   
--   In other words, there are effectively three modes of operation for output.
--   The first, simplest mode is used if the handler does nothing special.  In
--   this mode output is buffered and sent all at once; headers are not sent
--   until this time.  In this mode 'httpCloseOutput' may be useful to force
--   output to be sent before the handler returns, perhaps so that additional
--   time-consuming processing can be done.  This mode is easiest to use, in the
--   sense that it requires no support on the handler's part, but probably the
--   second mode should always be used instead.
--   
--   The second mode is used if the handler sets a Transfer-Encoding, for
--   example "chunked", and no Content-Length.  In this case headers are sent
--   immediately upon the first 'httpPut' or 'httpPutStr', and output is sent
--   as it is provided.  Output in this mode is transformed by 'httpPut' into
--   the appropriate transfer encoding.  Thus handler code need only specify a
--   transfer encoding, not actually implement that encoding itself.  This mode
--   is advantageous to allow user agents to begin displaying partial content as
--   it is received, and particularly useful when the content is quite large
--   or takes significant time to generate.  If you are unsure which mode to
--   use, it should probably be this one.
--   
--   The third mode is used if the handler sets a Content-Length and no
--   Transfer-Encoding.  In this case headers are again sent immediately upon
--   the first 'httpPut' or 'httpPutStr', and output is again sent as it is
--   provided.  Output in this mode is not transformed.  This may be more
--   efficient than the second mode if output is generated in many small pieces,
--   as it avoids computing and sending the length tags of the "chunked"
--   encoding.  However, it requires the content length to be known in advance
--   of actually sending any content.  It may be useful if you wish to have
--   direct-http validate that the handler is well-behaved in sending a binary
--   object of known size with no "garbage" inserted by spurious additional
--   puts.
httpPut :: (MonadHTTP m) => BS.ByteString -> m ()
httpPut bytestring = do
  requireOutputNotYetClosed
  markResponseHeadersUnmodifiable
  HTTPConnection {
      httpConnectionResponseContentBuffer = bufferMVar,
      httpConnectionResponseContentParameters = parametersMVar
    } <- getHTTPConnection
  buffer <- liftIO $ takeMVar bufferMVar
  parameters <- liftIO $ takeMVar parametersMVar
  parameters <- ensureResponseContentParametersInitialized parameters
  (buffer, parameters) <- case parameters of
    ResponseContentClosed -> httpThrow OutputAlreadyClosed
    ResponseContentBufferedIdentity -> do
      return (BS.append buffer bytestring, ResponseContentBufferedIdentity)
    ResponseContentUnbufferedIdentity lengthRemaining -> do
      sendResponseHeaders
      let lengthThisPut = BS.length bytestring
      if lengthThisPut > lengthRemaining
        then do
          liftIO $ putMVar parametersMVar ResponseContentClosed
          liftIO $ putMVar bufferMVar BS.empty
          httpThrow OutputAlreadyClosed
        else do
          let parameters' = ResponseContentUnbufferedIdentity
                             $ lengthRemaining - lengthThisPut
          send bytestring
          return (buffer, parameters')
    ResponseContentChunked -> do
      httpLog $ "Chunked not implemented."
      liftIO $ putMVar parametersMVar parameters
      liftIO $ putMVar bufferMVar buffer
      httpThrow UnexpectedEndOfInput
      -- TODO
  liftIO $ putMVar parametersMVar parameters
  liftIO $ putMVar bufferMVar buffer


ensureResponseContentParametersInitialized
  :: (MonadHTTP m)
  => ResponseContentParameters
  -> m ResponseContentParameters
ensureResponseContentParametersInitialized ResponseContentUninitialized = do
  maybeLengthString <- getResponseHeader HttpContentLength
  let maybeLength = case maybeLengthString of
                      Nothing -> Nothing
                      Just lengthString -> parseInt lengthString
  maybeTransferEncodingString <- getResponseHeader HttpTransferEncoding
  let (hasContent, chunked)
        = case (maybeLengthString, maybeTransferEncodingString) of
            (Nothing, Nothing) -> (False, False)
            (Just length, Nothing) -> (True, False)
            (Just length, Just encoding)
              | map toLower encoding == "identity" -> (True, False)
              | otherwise -> (True, True)
            (_, Just _) -> (True, True)
  if hasContent
    then if chunked
           then return $ ResponseContentChunked
           else return $ ResponseContentUnbufferedIdentity
                          $ fromJust maybeLength
    else return ResponseContentBufferedIdentity
ensureResponseContentParametersInitialized parameters = return parameters


flushResponseContent :: (MonadHTTP m) => m ()
flushResponseContent = do
  HTTPConnection {
      httpConnectionResponseContentBuffer = bufferMVar,
      httpConnectionResponseContentParameters = parametersMVar
    } <- getHTTPConnection
  buffer <- liftIO $ takeMVar bufferMVar
  parameters <- liftIO $ takeMVar parametersMVar
  parameters <- ensureResponseContentParametersInitialized parameters
  case parameters of
    ResponseContentClosed -> httpThrow OutputAlreadyClosed
    ResponseContentBufferedIdentity -> do
      send buffer
      return ()
    ResponseContentUnbufferedIdentity lengthRemaining -> do
      if lengthRemaining > 0
        then do
          liftIO $ putMVar parametersMVar ResponseContentClosed
          liftIO $ putMVar bufferMVar BS.empty
          httpThrow OutputIncomplete
        else return ()
    ResponseContentChunked -> do
      httpLog $ "Chunked not implemented."
      liftIO $ putMVar parametersMVar parameters
      liftIO $ putMVar bufferMVar buffer
      httpThrow UnexpectedEndOfInput
      -- TODO
  liftIO $ putMVar parametersMVar ResponseContentClosed
  liftIO $ putMVar bufferMVar BS.empty


send :: (MonadHTTP m) => ByteString -> m ()
send bytestring = do
  HTTPConnection { httpConnectionSocket = socket } <- getHTTPConnection
  httpCatch (liftIO $ Network.sendAll socket bytestring)
            (\e -> do
               return (e :: Exception.SomeException)
               return ())


-- | Appends text, encoded as UTF8, to the content of the HTTP response.  In
--   all respects this behaves as 'httpPut', but for the fact that it takes
--   text rather than binary data.
httpPutStr :: (MonadHTTP m) => String -> m ()
httpPutStr string = httpPut $ UTF8.fromString string


-- | Informs the web server and the user agent that the request has completed.
--   As side-effects, the response headers are sent if they have not yet been,
--   any unread input is discarded and no more can be read, and any unsent
--   output is sent.  This is implicitly called, if it has not already been,
--   after the handler returns; it may be useful within a handler if the
--   handler wishes to return results and then perform time-consuming
--   computations before exiting.  If output has already been closed, causes an
--   'OutputAlreadyClosed' exception.  If the response headers imply that there
--   will be a certain amount of data and there is not, causes an
--   'OutputIncomplete' exception.
httpCloseOutput :: (MonadHTTP m) => m ()
httpCloseOutput = do
  requireOutputNotYetClosed
  sendResponseHeaders
  flushResponseContent
  httpGet' Nothing True True
  return ()


-- | Returns whether it is possible to write more data; ie, whether output has
--   not yet been closed as by 'httpCloseOutput'.
httpIsWritable :: (MonadHTTP m) => m Bool
httpIsWritable = do
  HTTPConnection { httpConnectionResponseContentParameters = parametersMVar }
    <- getHTTPConnection
  parameters <- liftIO $ takeMVar parametersMVar
  parameters <- ensureResponseContentParametersInitialized parameters
  liftIO $ putMVar parametersMVar parameters
  return $ case parameters of
             ResponseContentClosed -> False
             _ -> True


requireResponseHeadersNotYetSent :: (MonadHTTP m) => m ()
requireResponseHeadersNotYetSent = do
  alreadySent <- responseHeadersSent
  if alreadySent
    then httpThrow ResponseHeadersAlreadySent
    else return ()


requireResponseHeadersModifiable :: (MonadHTTP m) => m ()
requireResponseHeadersModifiable = do
  modifiable <- responseHeadersModifiable
  if modifiable
    then return ()
    else httpThrow ResponseHeadersNotModifiable


requireOutputNotYetClosed :: (MonadHTTP m) => m ()
requireOutputNotYetClosed = do
  isWritable <- httpIsWritable
  case isWritable of
    False -> httpThrow OutputAlreadyClosed
    True -> return ()


-- | Throw an exception in any 'MonadHTTP' monad.
httpThrow
    :: (Exception.Exception e, MonadHTTP m)
    => e -- ^ The exception to throw.
    -> m a
httpThrow exception = implementationThrowHTTP exception


-- | Perform an action, with a given exception-handler action bound.  See
--   'Control.Exception.catch'.  The type of exception to catch is determined by the
--   type signature of the handler.
httpCatch
    :: (Exception.Exception e, MonadHTTP m)
    => m a -- ^ The action to run with the exception handler binding in scope.
    -> (e -> m a) -- ^ The exception handler to bind.
    -> m a
httpCatch action handler = implementationCatchHTTP action handler


-- | Block exceptions within an action, as per the discussion in 'Control.Exception'.
httpBlock
    :: (MonadHTTP m)
    => m a -- ^ The action to run with exceptions blocked.
    -> m a
httpBlock action = implementationBlockHTTP action


-- | Unblock exceptions within an action, as per the discussion in 'Control.Exception'.
httpUnblock
    :: (MonadHTTP m)
    => m a -- ^ The action to run with exceptions unblocked.
    -> m a
httpUnblock action = implementationUnblockHTTP action


-- | Acquire a resource, perform computation with it, and release it; see the description
--   of 'Control.Exception.bracket'.  If an exception is raised during the computation,
--   'httpBracket' will re-raise it after running the release function, having the effect
--   of propagating the exception further up the call stack.
httpBracket
    :: (MonadHTTP m)
    => m a -- ^ The action to acquire the resource.
    -> (a -> m b) -- ^ The action to release the resource.
    -> (a -> m c) -- ^ The action to perform using the resource.
    -> m c -- ^ The return value of the perform-action.
httpBracket acquire release perform = do
  httpBlock (do
           resource <- acquire
           result <- httpUnblock (perform resource) `httpOnException` (release resource)
           release resource
           return result)


-- | Perform an action, with a cleanup action bound to always occur; see the
--   description of 'Control.Exception.finally'.  If an exception is raised during the
--   computation, 'httpFinally' will re-raise it after running the cleanup action, having
--   the effect of propagating the exception further up the call stack.  If no
--   exception is raised, the cleanup action will be invoked after the main action is
--   performed.
httpFinally
    :: (MonadHTTP m)
    => m a -- ^ The action to perform.
    -> m b -- ^ The cleanup action.
    -> m a -- ^ The return value of the perform-action.
httpFinally perform cleanup = do
  httpBlock (do
           result <- httpUnblock perform `httpOnException` cleanup
           cleanup
           return result)


-- | Perform an action.  If any exceptions of the appropriate type occur within the
--   action, return 'Left' @exception@; otherwise, return 'Right' @result@.
httpTry
    :: (Exception.Exception e, MonadHTTP m)
    => m a -- ^ The action to perform.
    -> m (Either e a)
httpTry action = do
  httpCatch (do
           result <- action
           return $ Right result)
         (\exception -> return $ Left exception)


-- | As 'httpCatch', but with the arguments in the other order.
httpHandle
    :: (Exception.Exception e, MonadHTTP m)
    => (e -> m a) -- ^ The exception handler to bind.
    -> m a -- ^ The action to run with the exception handler binding in scope.
    -> m a
httpHandle handler action = httpCatch action handler


-- | Perform an action, with a cleanup action bound to occur if and only if an exception
--   is raised during the action; see the description of 'Control.Exception.finally'.
--   If an exception is raised during the computation, 'httpFinally' will re-raise it
--   after running the cleanup action, having the effect of propagating the exception
--   further up the call stack.  If no exception is raised, the cleanup action will not
--   be invoked.
httpOnException
    :: (MonadHTTP m)
    => m a -- ^ The action to perform.
    -> m b -- ^ The cleanup action.
    -> m a -- ^ The return value of the perform-action.
httpOnException action cleanup = do
  httpCatch action
         (\exception -> do
            cleanup
            httpThrow (exception :: Exception.SomeException))
