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
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS (toString, fromString)
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.Word
import Foreign.C.Error
import GHC.IO.Exception (IOErrorType(..))
import qualified Network.Socket as Network hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Network
import Prelude hiding (catch)
import System.Environment
import System.IO.Error (ioeGetErrorType)
import qualified System.IO.Error as System


-- | An opaque type representing the state of the HTTP server during a single
--   connection from a client.
data HTTPState = HTTPState {
    httpStateLogMVar :: MVar (),
    httpStateSocket :: Network.Socket,
    httpStatePeer :: Network.SockAddr
  }


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


-- | Takes a forking primitive, such as 'forkIO' or 'forkOS', and a handler, and
--   concurrently accepts requests from the web server, forking with the primitive
--   and invoking the handler in the forked thread inside the 'HTTP' monad for each
--   one.
--   
--   If the daemonize flag is True, first closes the standard IO streams and moves
--   the process into the background, doing all the usual Unix things to make it run
--   as a daemon henceforth.  This is optional because it might be useful to turn it
--   off for debugging purposes.
--   
--   It is valid to use a custom forking primitive, such as one that attempts to pool
--   OS threads, but the primitive must actually provide concurrency - otherwise there
--   will be a deadlock.  There is no support for single-threaded operation.
--   
--   Note that although there is no mechanism to substitute another type of monad for
--   HTTP, you can enter your own monad within the handler, much as you would enter
--   your own monad within IO.  You simply have to implement the 'MonadHTTP' class.
--   
--   Any exceptions not caught within the handler are caught by 'concurrentAcceptLoop',
--   and cause the termination of that handler, but not of the accept loop.
acceptLoop
    :: Bool
    -- ^ A flag which indicates whether to daemonize.
    -> (IO () -> IO ThreadId)
    -- ^ A forking primitive, typically either 'forkIO' or 'forkOS'.
    -> (HTTP ())
    -- ^ A handler which is invoked once for each incoming connection.
    -> IO ()
    -- ^ Never actually returns.
acceptLoop shouldDaemonize fork handler = do
  listenSocket <- createListenSocket
  if shouldDaemonize
    then do
      daemonize
    else return ()
  logMVar <- newMVar ()
  let acceptLoop' = do
        (socket, peer) <- Network.accept listenSocket
        requestLoop logMVar socket peer fork handler
        acceptLoop'
  acceptLoop'


daemonize :: IO ()
daemonize = do
  putStrLn $ "Pretend-daemonizing."


createListenSocket :: IO Network.Socket
createListenSocket = do
  listenSocket <- Network.socket Network.AF_INET
                                 Network.Stream
                                 Network.defaultProtocol
  Network.bindSocket listenSocket $ Network.SockAddrInet 80 Network.iNADDR_ANY
  Network.listen listenSocket 1024
  return listenSocket


requestLoop :: MVar ()
            -> Network.Socket
            -> Network.SockAddr
            -> (IO () -> IO ThreadId)
            -> HTTP ()
            -> IO ()
requestLoop logMVar socket peer fork handler = do
  maybeHeaders <- recvHeaders socket
  case maybeHeaders of
    Nothing -> do
      liftIO $ Exception.catch (Network.sClose socket)
                               (\error -> do
                                  return $ error :: IO Exception.IOException
                                  return ())
      return ()
    Just headers -> do
      let state = HTTPState {
                      httpStateLogMVar = logMVar,
                      httpStateSocket = socket,
                      httpStatePeer = peer
                    }
      fork $ do
        Exception.catch
          (runReaderT handler state)
          (\error -> flip runReaderT state $ do
            httpLog $ "Uncaught exception: "
                      ++ (show (error :: Exception.SomeException))
            httpCloseOutput)
        return ()
      requestLoop logMVar socket peer fork handler


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


printCookies :: [Cookie] -> String
printCookies cookies =
    let printCookie cookie
            = intercalate ";" $ map printNameValuePair $ nameValuePairs cookie
        printNameValuePair (name, Nothing) = name
        printNameValuePair (name, Just value)
            = name ++ "=" ++ value
        {- Safari doesn't like this.
            = if isValidCookieToken value
                then name ++ "=" ++ value
                else name ++ "=\"" ++ escape value ++ "\""
         -}
        escape "" = ""
        escape ('\\':rest) = "\\\\" ++ escape rest
        escape ('\"':rest) = "\\\"" ++ escape rest
        escape (c:rest) = [c] ++ escape rest
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
    in intercalate "," $ map printCookie cookies


parseInt :: String -> Maybe Int
parseInt string =
    if (length string > 0) && (all isDigit string)
      then Just $ let accumulate "" accumulator = accumulator
                      accumulate (n:rest) accumulator
                                 = accumulate rest $ accumulator * 10 + digitToInt n
                  in accumulate string 0
      else Nothing


recvHeaders :: Network.Socket -> IO (Maybe (Map Header String))
recvHeaders socket = do
  byteString <- liftIO $ Network.recv socket 1
  case BS.length byteString of
    0 -> return Nothing
    1 -> do
      return $ Just $ Map.empty
    _ -> return Nothing
  -- TODO


-- | Logs a message using the web server's logging facility.
httpLog :: (MonadHTTP m) => String -> m ()
httpLog message = do
  HTTPState { httpStateLogMVar = logMVar } <- getHTTPState
  liftIO $ withMVar logMVar (\() -> putStrLn message)


-- | Headers are classified by HTTP/1.1 as request headers, response headers, or
--   entity headers.
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
    | HttpReferer
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
    | HttpExtensionHeader String
    -- | Nonstandard headers
    | HttpConnection
    | HttpCookie
    | HttpSetCookie
      deriving (Eq, Ord)


instance Show Header where 
    show header = fromHeader header


data HeaderType = RequestHeader
                | ResponseHeader
                | EntityHeader
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
headerType HttpReferer = RequestHeader
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
headerType HttpConnection = RequestHeader
headerType HttpCookie = RequestHeader
headerType HttpSetCookie = ResponseHeader


fromHeader :: Header -> String
fromHeader HttpAccept = "Accept"
fromHeader HttpAcceptCharset = "Accept-Charset"
fromHeader HttpAcceptEncoding = "Accept-Encoding"
fromHeader HttpAcceptLanguage = "Accept-Language"
fromHeader HttpAuthorization = "Authorization"
fromHeader HttpExpect = "Expect"
fromHeader HttpFrom = "From"
fromHeader HttpHost = "Host"
fromHeader HttpIfMatch = "If-Match"
fromHeader HttpIfModifiedSince = "If-Modified-Since"
fromHeader HttpIfNoneMatch = "If-None-Match"
fromHeader HttpIfRange = "If-Range"
fromHeader HttpIfUnmodifiedSince = "If-Unmodified-Since"
fromHeader HttpMaxForwards = "Max-Forwards"
fromHeader HttpProxyAuthorization = "Proxy-Authorization"
fromHeader HttpRange = "Range"
fromHeader HttpReferer = "Referer"
fromHeader HttpTE = "TE"
fromHeader HttpUserAgent = "User-Agent"
fromHeader HttpAcceptRanges = "Accept-Ranges"
fromHeader HttpAge = "Age"
fromHeader HttpETag = "ETag"
fromHeader HttpLocation = "Location"
fromHeader HttpProxyAuthenticate = "Proxy-Authenticate"
fromHeader HttpRetryAfter = "Retry-After"
fromHeader HttpServer = "Server"
fromHeader HttpVary = "Vary"
fromHeader HttpWWWAuthenticate = "WWW-Authenticate"
fromHeader HttpAllow = "Allow"
fromHeader HttpContentEncoding = "Content-Encoding"
fromHeader HttpContentLanguage = "Content-Language"
fromHeader HttpContentLength = "Content-Length"
fromHeader HttpContentLocation = "Content-Location"
fromHeader HttpContentMD5 = "Content-MD5"
fromHeader HttpContentRange = "Content-Range"
fromHeader HttpContentType = "Content-Type"
fromHeader HttpExpires = "Expires"
fromHeader HttpLastModified = "Last-Modified"
fromHeader (HttpExtensionHeader name) = name
fromHeader HttpConnection = "Connection"
fromHeader HttpCookie = "Cookie"
fromHeader HttpSetCookie = "Set-Cookie"


toHeader :: String -> Header
toHeader "Accept" = HttpAccept
toHeader "Accept-Charset" = HttpAcceptCharset
toHeader "Accept-Encoding" = HttpAcceptEncoding
toHeader "Accept-Language" = HttpAcceptLanguage
toHeader "Authorization" = HttpAuthorization
toHeader "Expect" = HttpExpect
toHeader "From" = HttpFrom
toHeader "Host" = HttpHost
toHeader "If-Match" = HttpIfMatch
toHeader "If-Modified-Since" = HttpIfModifiedSince
toHeader "If-None-Match" = HttpIfNoneMatch
toHeader "If-Range" = HttpIfRange
toHeader "If-Unmodified-Since" = HttpIfUnmodifiedSince
toHeader "Max-Forwards" = HttpMaxForwards
toHeader "Proxy-Authorization" = HttpProxyAuthorization
toHeader "Range" = HttpRange
toHeader "Referer" = HttpReferer
toHeader "TE" = HttpTE
toHeader "User-Agent" = HttpUserAgent
toHeader "Accept-Ranges" = HttpAcceptRanges
toHeader "Age" = HttpAge
toHeader "ETag" = HttpETag
toHeader "Location" = HttpLocation
toHeader "Proxy-Authenticate" = HttpProxyAuthenticate
toHeader "Retry-After" = HttpRetryAfter
toHeader "Server" = HttpServer
toHeader "Vary" = HttpVary
toHeader "WWW-Authenticate" = HttpWWWAuthenticate
toHeader "Allow" = HttpAllow
toHeader "Content-Encoding" = HttpContentEncoding
toHeader "Content-Language" = HttpContentLanguage
toHeader "Content-Length" = HttpContentLength
toHeader "Content-Location" = HttpContentLocation
toHeader "Content-MD5" = HttpContentMD5
toHeader "Content-Range" = HttpContentRange
toHeader "Content-Type" = HttpContentType
toHeader "Expires" = HttpExpires
toHeader "Last-Modified" = HttpLastModified
toHeader "Connection" = HttpConnection
toHeader "Cookie" = HttpCookie
toHeader "Set-Cookie" = HttpSetCookie
toHeader name = HttpExtensionHeader name


requestVariableNameIsHeader :: String -> Bool
requestVariableNameIsHeader name = (length name > 5) && (take 5 name == "HTTP_")


requestVariableNameToHeaderName :: String -> Maybe String
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
             in Just headerName
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
requestVariableNameToHeader "HTTP_REFERER" = Just HttpReferer
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
requestVariableNameToHeader "HTTP_CONNECTION" = Just HttpConnection
requestVariableNameToHeader "HTTP_COOKIE" = Just HttpCookie
requestVariableNameToHeader name
    = if requestVariableNameIsHeader name
        then Just $ HttpExtensionHeader $ fromJust $ requestVariableNameToHeaderName name
        else Nothing


isValidInResponse :: Header -> Bool
isValidInResponse header = (headerType header == ResponseHeader)
                           || (headerType header == EntityHeader)


-- | Queries the value from the web server of the CGI/1.1 request variable with the
--   given name for this request.  This interface is provided as a convenience to
--   programs which were originally written against the CGI or FastCGI APIs.
getRequestVariable
    :: (MonadHTTP m)
    => String -- ^ The name of the request variable to query.
    -> m (Maybe String) -- ^ The value of the request variable, if the web server
                        --   provided one.
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
getRequestVariable _ = return Nothing


-- | Returns an association list of name-value pairs of all the CGI/1.1 request
--   variables from the web server.  This interface is provided as a convenience
--   to programs which were originally written against the CGI or FastCGI APIs.
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
             "CONTENT_LENGTH", "CONTENT_TYPE"]
  return $ map fromJust $ filter isJust result


-- | Queries the value from the user agent of the given HTTP/1.1 header.
getRequestHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to query.  Must be a request or entity header.
    -> m (Maybe String) -- ^ The value of the header, if the user agent provided one.
getRequestHeader header = do
  return Nothing
  -- TODO


-- | Returns an association list of name-value pairs of all the HTTP/1.1 request or
--   entity headers from the user agent.
getAllRequestHeaders :: (MonadHTTP m) => m [(Header, String)]
getAllRequestHeaders = do
  return []
  -- TODO


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


-- | Return the document root, as provided by the web server, if it was provided.
getDocumentRoot :: (MonadHTTP m) => m (Maybe String)
getDocumentRoot = do
  return Nothing
  -- TODO


-- | Return the gateway interface, as provided by the web server, if it was provided.
getGatewayInterface :: (MonadHTTP m) => m (Maybe String)
getGatewayInterface = do
  return Nothing
  -- TODO


-- | Return the path info, as provided by the web server, if it was provided.
getPathInfo :: (MonadHTTP m) => m (Maybe String)
getPathInfo = do
  return Nothing
  -- TODO


-- | Return the path-translated value, as provided by the web server, if it was provided.
getPathTranslated :: (MonadHTTP m) => m (Maybe String)
getPathTranslated = do
  return Nothing
  -- TODO


-- | Return the query string, as provided by the web server, if it was provided.
getQueryString :: (MonadHTTP m) => m (Maybe String)
getQueryString = do
  return Nothing
  -- TODO


-- | Return the redirect status, as provided by the web server, if it was provided.
getRedirectStatus :: (MonadHTTP m) => m (Maybe Int)
getRedirectStatus = do
  return Nothing
  -- TODO


-- | Return the redirect URI, as provided by the web server, if it was provided.
getRedirectURI :: (MonadHTTP m) => m (Maybe String)
getRedirectURI = do
  return Nothing
  -- TODO


-- | Return the remote address, as provided by the web server, if it was provided.
getRemoteAddress :: (MonadHTTP m) => m (Maybe Network.HostAddress)
getRemoteAddress = do
  return Nothing
  -- TODO


-- | Return the remote port, as provided by the web server, if it was provided.
getRemotePort :: (MonadHTTP m) => m (Maybe Int)
getRemotePort = do
  return Nothing
  -- TODO


-- | Return the remote hostname, as provided by the web server, if it was provided.
getRemoteHost :: (MonadHTTP m) => m (Maybe String)
getRemoteHost = do
  return Nothing
  -- TODO


-- | Return the remote ident value, as provided by the web server, if it was provided.
getRemoteIdent :: (MonadHTTP m) => m (Maybe String)
getRemoteIdent = do
  return Nothing
  -- TODO


-- | Return the remote user name, as provided by the web server, if it was provided.
getRemoteUser :: (MonadHTTP m) => m (Maybe String)
getRemoteUser = do
  return Nothing
  -- TODO


-- | Return the request method, as provided by the web server, if it was provided.
getRequestMethod :: (MonadHTTP m) => m (Maybe String)
getRequestMethod = do
  return Nothing
  -- TODO


-- | Return the request URI, as provided by the web server, if it was provided.
getRequestURI :: (MonadHTTP m) => m (Maybe String)
getRequestURI = do
  return Nothing
  -- TODO


-- | Return the script filename, as provided by the web server, if it was provided.
getScriptFilename :: (MonadHTTP m) => m (Maybe String)
getScriptFilename = do
  return Nothing
  -- TODO


-- | Return the script name, as provided by the web server, if it was provided.
getScriptName :: (MonadHTTP m) => m (Maybe String)
getScriptName = do
  return Nothing
  -- TODO


-- | Return the server address, as provided by the web server, if it was provided.
getServerAddress :: (MonadHTTP m) => m (Maybe Network.HostAddress)
getServerAddress = do
  return Nothing
  -- TODO


-- | Return the server name, as provided by the web server, if it was provided.
getServerName :: (MonadHTTP m) => m (Maybe String)
getServerName = do
  return Nothing
  -- TODO


-- | Return the server port, as provided by the web server, if it was provided.
getServerPort :: (MonadHTTP m) => m (Maybe Int)
getServerPort = do
  return Nothing
  -- TODO


-- | Return the server protocol, as provided by the web server, if it was provided.
getServerProtocol :: (MonadHTTP m) => m (Maybe String)
getServerProtocol = do
  return Nothing
  -- TODO


-- | Return the server software name and version, as provided by the web server, if
--   it was provided.
getServerSoftware :: (MonadHTTP m) => m (Maybe String)
getServerSoftware = do
  return Nothing
  -- TODO


-- | Return the authentication type, as provided by the web server, if it was provided.
getAuthenticationType :: (MonadHTTP m) => m (Maybe String)
getAuthenticationType = do
  return Nothing
  -- TODO


-- | Return the content length, as provided by the web server, if it was provided.
getContentLength :: (MonadHTTP m) => m (Maybe Int)
getContentLength = do
  return Nothing
  -- TODO


-- | Return the content type, as provided by the web server, if it was provided.
getContentType :: (MonadHTTP m) => m (Maybe String)
getContentType = do
  return Nothing
  -- TODO


-- | Reads up to a specified amount of data from the input stream of the current request,
--   and interprets it as binary data.  This is the content data of the HTTP request,
--   if any.  If input has been closed, returns an empty bytestring.  If insufficient
--   input is available, blocks until there is enough.  If output has been closed,
--   causes an 'OutputAlreadyClosed' exception.
httpGet :: (MonadHTTP m) => Int -> m BS.ByteString
httpGet size = httpGet' size False


-- | Reads up to a specified amount of data from the input stream of the curent request,
--   and interprets it as binary data.  This is the content data of the HTTP request,
--   if any.  If input has been closed, returns an empty bytestring.  If insufficient
--   input is available, returns any input which is immediately available, or an empty
--   bytestring if there is none, never blocking.  If output has been closed, causes an
--   'OutputAlreadyClosed' exception.
httpGetNonBlocking :: (MonadHTTP m) => Int -> m BS.ByteString
httpGetNonBlocking size = httpGet' size True


httpGet' :: (MonadHTTP m) => Int -> Bool -> m BS.ByteString
httpGet' size nonBlocking = do
  requireOutputNotYetClosed
  {-
  HTTPState { request = Just request } <- getHTTPState
  extendStdinStreamBufferToLength size nonBlocking
  stdinStreamBuffer <- liftIO $ takeMVar $ stdinStreamBufferMVar request
  if size <= BS.length stdinStreamBuffer
    then do
      let result = BS.take size stdinStreamBuffer
          remainder = BS.drop size stdinStreamBuffer
      liftIO $ putMVar (stdinStreamBufferMVar request) remainder
      return result
    else do
      liftIO $ putMVar (stdinStreamBufferMVar request) BS.empty
      return stdinStreamBuffer
      -}
  return BS.empty
  -- TODO


-- | Reads all remaining data from the input stream of the current request, and
--   interprets it as binary data.  This is the content data of the HTTP request, if
--   any.  Blocks until all input has been read.  If input has been closed, returns an
--   empty bytestring.  If output has been closed, causes an 'OutputAlreadyClosed'
--   exception.
httpGetContents :: (MonadHTTP m) => m BS.ByteString
httpGetContents = do
  requireOutputNotYetClosed
  {-
  HTTPState { request = Just request } <- getHTTPState
  let extend = do
        stdinStreamBuffer <- liftIO $ readMVar $ stdinStreamBufferMVar request
        extendStdinStreamBufferToLength (BS.length stdinStreamBuffer + 1) False
        stdinStreamClosed <- liftIO $ readMVar $ stdinStreamClosedMVar request
        if stdinStreamClosed
          then do
            stdinStreamBuffer
                <- liftIO $ swapMVar (stdinStreamBufferMVar request) BS.empty
            return stdinStreamBuffer
          else extend
  extend
  -}
  return BS.empty
  -- TODO


-- | Returns whether the input stream of the current request potentially has data
--   remaining, either in the buffer or yet to be read.  This is the content data of
--   the HTTP request, if any.
httpIsReadable :: (MonadHTTP m) => m Bool
httpIsReadable = do
  {-
  HTTPState { request = Just request } <- getHTTPState
  stdinStreamBuffer <- liftIO $ readMVar $ stdinStreamBufferMVar request
  if BS.length stdinStreamBuffer > 0
    then return True
    else do
      stdinStreamClosed <- liftIO $ readMVar $ stdinStreamClosedMVar request
      requestEnded <- liftIO $ readMVar $ requestEndedMVar request
      return $ (not stdinStreamClosed) && (not requestEnded)
   -}
  return False
  -- TODO


extendStdinStreamBufferToLength :: (MonadHTTP m) => Int -> Bool -> m ()
extendStdinStreamBufferToLength desiredLength nonBlocking = do
  {-
  HTTPState { request = Just request } <- getHTTPState
  stdinStreamBuffer <- liftIO $ takeMVar $ stdinStreamBufferMVar request
  let extend bufferSoFar = do
        if BS.length bufferSoFar >= desiredLength
          then liftIO $ putMVar (stdinStreamBufferMVar request) bufferSoFar
          else do
            maybeRecord <- if nonBlocking
                             then do
                               isEmpty <- liftIO $ isEmptyChan $ requestChannel request
                               if isEmpty
                                 then return Nothing
                                 else do
                                   record <- liftIO $ readChan $ requestChannel request
                                   return $ Just record
                             else do
                               record <- liftIO $ readChan $ requestChannel request
                               return $ Just record
            case maybeRecord of
              Nothing -> liftIO $ putMVar (stdinStreamBufferMVar request) bufferSoFar
              Just record
                -> case recordType record of
                     StdinRecord -> do
                       case BS.length $ recordContent record of
                         0 -> do
                           liftIO $ swapMVar (stdinStreamClosedMVar request) True
                           liftIO $ putMVar (stdinStreamBufferMVar request) bufferSoFar
                         _ -> do
                           extend $ BS.append bufferSoFar $ recordContent record
                     _ -> do
                       httpLog $ "Ignoring record of unexpected type "
                              ++ (show $ recordType record)
  extend stdinStreamBuffer
  -}
  return ()
  -- TODO


-- | Sets the response status which will be sent with the response headers.  If the
--   response headers have already been sent, causes a 'ResponseHeadersAlreadySent'
--   exception.
setResponseStatus
    :: (MonadHTTP m)
    => Int -- ^ The HTTP/1.1 status code to set.
    -> m ()
setResponseStatus status = do
  requireResponseHeadersNotYetSent
  {-
  HTTPState { request = Just request } <- getHTTPState
  liftIO $ swapMVar (responseStatusMVar request) status
  -}
  return ()
  -- TODO
      


-- | Returns the response status which will be or has been sent with the response
--   headers.
getResponseStatus
    :: (MonadHTTP m)
    => m Int -- ^ The HTTP/1.1 status code.
getResponseStatus = do
  {-
  HTTPState { request = Just request } <- getHTTPState
  liftIO $ readMVar (responseStatusMVar request)
  -}
  return 200
  -- TODO


-- | Sets the given 'HttpHeader' response header to the given string value, overriding
--   any value which has previously been set.  If the response headers have already
--   been sent, causes a 'ResponseHeadersAlreadySent' exception.  If the header is not
--   an HTTP/1.1 or extension response or entity header, ie, is not valid as part of
--   a response, causes a 'NotAResponseHeader' exception.
--   
--   If a value is set for the 'HttpSetCookie' header, this overrides all cookies set
--   for this request with 'setCookie'.
setResponseHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to set.  Must be a response header or an entity header.
    -> String -- ^ The value to set.
    -> m ()
setResponseHeader header value = do
  requireResponseHeadersNotYetSent
  if isValidInResponse header
    then do
      {-
      HTTPState { request = Just request } <- getHTTPState
      responseHeaderMap <- liftIO $ takeMVar $ responseHeaderMapMVar request
      let responseHeaderMap' = Map.insert header value responseHeaderMap
      liftIO $ putMVar (responseHeaderMapMVar request) responseHeaderMap'
      -}
      return ()
    else httpThrow $ NotAResponseHeader header
  -- TODO


-- | Causes the given 'HttpHeader' response header not to be sent, overriding any value
--   which has previously been set.  If the response headers have already been sent,
--   causes a 'ResponseHeadersAlreadySent' exception.  If the header is not an HTTP/1.1
--   or extension response or entity header, ie, is not valid as part of a response,
--   causes a 'NotAResponseHeader' exception.
--   
--   Does not prevent the 'HttpSetCookie' header from being sent if cookies have been
--   set for this request with 'setCookie'.
unsetResponseHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to unset.  Must be a response header or an entity header.
    -> m ()
unsetResponseHeader header = do
  requireResponseHeadersNotYetSent
  if isValidInResponse header
    then do
      {-
      HTTPState { request = Just request } <- getHTTPState
      responseHeaderMap <- liftIO $ takeMVar $ responseHeaderMapMVar request
      let responseHeaderMap' = Map.delete header responseHeaderMap
      liftIO $ putMVar (responseHeaderMapMVar request) responseHeaderMap'
      -}
      return ()
    else httpThrow $ NotAResponseHeader header
  -- TODO


-- | Returns the value of the given header which will be or has been sent with the
--   response headers.  If the header is not an HTTP/1.1 or extension response or entity
--   header, ie, is not valid as part of a response, causes a 'NotAResponseHeader'
--   exception.
getResponseHeader
    :: (MonadHTTP m)
    => Header -- ^ The header to query.  Must be a response header or an entity
              --   header.
    -> m (Maybe String) -- ^ The value of the queried header.
getResponseHeader header = do
  requireResponseHeadersNotYetSent
  if isValidInResponse header
    then do
      {-
      HTTPState { request = Just request } <- getHTTPState
      responseHeaderMap <- liftIO $ readMVar $ responseHeaderMapMVar request
      return $ Map.lookup header responseHeaderMap
      -}
      return Nothing
    else httpThrow $ NotAResponseHeader header
  -- TODO


-- | Causes the user agent to record the given cookie and send it back with future
--   loads of this page.  Does not take effect instantly, but rather when headers are
--   sent.  Cookies are set in accordance with RFC 2109.
--   If an @HttpCookie@ header is set for this request by a call to 'setResponseHeader',
--   this function has no effect.
--   If the response headers have already been sent,
--   causes a 'ResponseHeadersAlreadySent' exception.
--   If the name is not a possible name for a cookie, causes a 'CookieNameInvalid'
--   exception.
setCookie
    :: (MonadHTTP m)
    => Cookie -- ^ The cookie to set.
    -> m ()
setCookie cookie = do
  requireResponseHeadersNotYetSent
  requireValidCookieName $ cookieName cookie
  {-
  HTTPState { request = Just request } <- getHTTPState
  responseCookieMap <- liftIO $ takeMVar $ responseCookieMapMVar request
  let responseCookieMap' = Map.insert (cookieName cookie) cookie responseCookieMap
  liftIO $ putMVar (responseCookieMapMVar request) responseCookieMap'
  -}
  return ()
  -- TODO


-- | Causes the user agent to unset any cookie applicable to this page with the
--   given name.  Does not take effect instantly, but rather when headers are sent.
--   If an @HttpCookie@ header is set for this request by a call to 'setResponseHeader',
--   this function has no effect.
--   If the response headers have already been sent,
--   causes a 'ResponseHeadersAlreadySent' exception.
--   If the name is not a possible name for a cookie, causes a 'CookieNameInvalid'
--   exception.
unsetCookie
    :: (MonadHTTP m)
    => String -- ^ The name of the cookie to unset.
    -> m ()
unsetCookie name = do
  requireResponseHeadersNotYetSent
  requireValidCookieName name
  {-
  HTTPState { request = Just request } <- getHTTPState
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
    | OutputAlreadyClosed
      -- ^ An exception thrown by operations which produce output when output has
      --   been closed, as by 'httpCloseOutput'.
    | NotAResponseHeader Header
      -- ^ An exception thrown by operations which are given a header that does not
      --   meet their requirement of being valid in a response.
    | CookieNameInvalid String
      -- ^ An exception thrown by operations which are given cookie names that do not
      --   meet the appropriate syntax requirements.
      deriving (Show, Typeable)


instance Exception.Exception HTTPException


-- | Sets the HTTP/1.1 return status to 301 and sets the 'HttpLocation' header to
--   the provided URL.  This has the effect of issuing a permanent redirect to the
--   user agent.  Permanent redirects, as opposed to temporary redirects, may cause
--   bookmarks or incoming links to be updated.  If the response headers have already
--   been sent, causes a 'ResponseHeadersAlreadySent' exception.
permanentRedirect
    :: (MonadHTTP m)
    => String -- ^ The URL to redirect to, as a string.
    -> m ()
permanentRedirect url = do
  setResponseStatus 301
  setResponseHeader HttpLocation url


-- | Sets the HTTP/1.1 return status to 303 and sets the 'HttpLocation' header to
--   the provided URL.  This has the effect of issuing a see-other or "temporary"
--   redirect to the user agent.  Temporary redirects, as opposed to permanent redirects,
--   do not cause bookmarks or incoming links to be updated.  If the response headers
--   have already been sent, causes a 'ResponseHeadersAlreadySent' exception.
seeOtherRedirect
    :: (MonadHTTP m)
    => String -- ^ The URL to redirect to, as a string.
    -> m ()
seeOtherRedirect url = do
  setResponseStatus 303
  setResponseHeader HttpLocation url


-- | Ensures that the response headers have been sent.  If they are already sent, does
--   nothing.  If output has already been closed, causes an 'OutputAlreadyClosed'
--   exception.
sendResponseHeaders :: (MonadHTTP m) => m ()
sendResponseHeaders = do
  requireOutputNotYetClosed
  {-
  HTTPState { request = Just request } <- getHTTPState
  alreadySent <- liftIO $ takeMVar $ responseHeadersSentMVar request
  if not alreadySent
    then do
      responseStatus <- liftIO $ readMVar $ responseStatusMVar request
      responseHeaderMap <- liftIO $ readMVar $ responseHeaderMapMVar request
      responseCookieMap <- liftIO $ readMVar $ responseCookieMapMVar request
      let nameValuePairs = [("Status", (show responseStatus))]
                           ++ (map (\key -> (fromHeader key,
                                             fromJust $ Map.lookup key responseHeaderMap))
                                   $ Map.keys responseHeaderMap)
                           ++ (if (isNothing $ Map.lookup HttpSetCookie
                                                          responseHeaderMap)
                                  && (length (Map.elems responseCookieMap) > 0)
                                 then [("Set-Cookie", setCookieValue)]
                                 else [])
          setCookieValue = printCookies $ Map.elems responseCookieMap
          bytestrings
              = (map (\(name, value) -> BS.fromString $ name ++ ": " ++ value ++ "\r\n")
                     nameValuePairs)
                ++ [BS.fromString "\r\n"]
          buffer = foldl BS.append BS.empty bytestrings
      sendBuffer buffer
    else return ()
  liftIO $ putMVar (responseHeadersSentMVar request) True
  -}
  return ()
  -- TODO


-- | Returns whether the response headers have been sent.
responseHeadersSent :: (MonadHTTP m) => m Bool
responseHeadersSent = do
  {-
  HTTPState { request = Just request } <- getHTTPState
  liftIO $ readMVar $ responseHeadersSentMVar request
  -}
  return False
  -- TODO


-- | Sends data.  This is the content data of the HTTP response.  If the response
--   headers have not been sent, first sends them.  If output has already been closed,
--   causes an 'OutputAlreadyClosed' exception.
httpPut :: (MonadHTTP m) => BS.ByteString -> m ()
httpPut buffer = do
  requireOutputNotYetClosed
  sendResponseHeaders
  return ()
  -- TODO


-- | Sends text, encoded as UTF-8.  This is the content data of the HTTP response.
--   if the response headers have not been sent, first sends them.  If output has
--   already been closed, causes an 'OutputAlreadyClosed' exception.
httpPutStr :: (MonadHTTP m) => String -> m ()
httpPutStr string = httpPut $ BS.fromString string


-- | Informs the web server and the user agent that the request has completed.  As
--   side-effects, the response headers are sent if they have not yet been, and
--   any unread input is discarded and no more can be read.  This is
--   implicitly called, if it has not already been, after the handler returns; it
--   may be useful within a handler if the handler wishes to return results and then
--   perform time-consuming computations before exiting.  If output has already been
--   closed, causes an 'OutputAlreadyClosed' exception.
httpCloseOutput :: (MonadHTTP m) => m ()
httpCloseOutput = do
  requireOutputNotYetClosed
  sendResponseHeaders
  terminateRequest
  -- TODO


-- | Returns whether it is possible to write more data; ie, whether output has not
--   yet been closed as by 'httpCloseOutput'.
httpIsWritable :: (MonadHTTP m) => m Bool
httpIsWritable = do
  {-
  HTTPState { request = Just request } <- getHTTPState
  requestEnded <- liftIO $ readMVar $ requestEndedMVar request
  return $ not requestEnded
  -}
  return False
  -- TODO


terminateRequest :: (MonadHTTP m) => m ()
terminateRequest = do
  return ()
  -- TODO


requireResponseHeadersNotYetSent :: (MonadHTTP m) => m ()
requireResponseHeadersNotYetSent = do
  {-
  HTTPState { request = Just request } <- getHTTPState
  alreadySent <- liftIO $ readMVar $ responseHeadersSentMVar request
  if alreadySent
    then httpThrow ResponseHeadersAlreadySent
    else return ()
   -}
  return ()
  -- TODO


requireOutputNotYetClosed :: (MonadHTTP m) => m ()
requireOutputNotYetClosed = do
  {-
  HTTPState { request = Just request } <- getHTTPState
  requestEnded <- liftIO $ readMVar $ requestEndedMVar request
  if requestEnded
    then httpThrow OutputAlreadyClosed
    else return ()
   -}
  return ()
  -- TODO


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
