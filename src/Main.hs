{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (forever, void)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hPutStrLn, stderr)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "errors.garden starting on port " ++ show port
  runServer port

runServer :: Int -> IO ()
runServer port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral port) 0)
  listen sock 128
  forever $ do
    (conn, _) <- accept sock
    void $ forkIO $ handleConnection conn

handleConnection :: Socket -> IO ()
handleConnection conn = do
  result <- try $ do
    request <- recv conn 4096
    response <- handleRequest request
    sendAll conn response
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> pure ()
  close conn

handleRequest :: ByteString -> IO ByteString
handleRequest raw = do
  let (method, path) = parseRequest raw
  timestamp <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
  hPutStrLn stderr $ timestamp ++ " " ++ BS.unpack method ++ " " ++ BS.unpack path
  pure $ case method of
    "GET" -> routeGet path
    _     -> resp 405 "text/plain" "Method Not Allowed"

parseRequest :: ByteString -> (ByteString, ByteString)
parseRequest raw = case BS.words (head $ BS.lines raw) of
  (m:p:_) -> (m, BS.takeWhile (/= '?') p)
  _ -> ("", "/")

routeGet :: ByteString -> ByteString
routeGet path = case BS.unpack path of
  "/" -> resp 200 "text/html; charset=utf-8" homePage
  "/errors" -> resp 200 "text/html; charset=utf-8" errorsPage
  "/errors/" -> resp 200 "text/html; charset=utf-8" errorsPage
  "/ping" -> resp 200 "application/json" "{\"ok\":true}"
  '/':'h':'t':'t':'p':'/':c -> httpErr c
  '/':'w':'s':'/':c -> wsErr c
  '/':'g':'r':'p':'c':'/':c -> grpcErr c
  '/':'m':'c':'p':'/':c -> mcpErr c
  '/':'g':'q':'l':'/':c -> gqlErr c
  '/':'a':'w':'s':'/':c -> awsErr c
  '/':'s':'t':'r':'i':'p':'e':'/':c -> stripeErr c
  '/':'f':'a':'s':'t':'a':'p':'i':'/':c -> fastapiErr c
  '/':'p':'y':'d':'a':'n':'t':'i':'c':'/':c -> pydanticErr c
  '/':'o':'a':'u':'t':'h':'/':c -> oauthErr c
  '/':'p':'o':'s':'t':'g':'r':'e':'s':'/':c -> pgErr c
  '/':'m':'y':'s':'q':'l':'/':c -> mysqlErr c
  '/':'r':'e':'d':'i':'s':'/':c -> redisErr c
  '/':'m':'o':'n':'g':'o':'/':c -> mongoErr c
  '/':'e':'l':'a':'s':'t':'i':'c':'/':c -> elasticErr c
  '/':'k':'8':'s':'/':c -> k8sErr c
  '/':'d':'o':'c':'k':'e':'r':'/':c -> dockerErr c
  '/':'f':'i':'r':'e':'b':'a':'s':'e':'/':c -> firebaseErr c
  '/':'t':'w':'i':'l':'i':'o':'/':c -> twilioErr c
  '/':'s':'e':'n':'d':'g':'r':'i':'d':'/':c -> sendgridErr c
  '/':'c':'l':'o':'u':'d':'f':'l':'a':'r':'e':'/':c -> cloudflareErr c
  '/':'v':'e':'r':'c':'e':'l':'/':c -> vercelErr c
  '/':'s':'u':'p':'a':'b':'a':'s':'e':'/':c -> supabaseErr c
  '/':'h':'t':'c':'p':'c':'p':'/':c -> htcpcpErr c
  '/':'c':'o':'f':'f':'e':'e':'/':c -> htcpcpErr c
  _ -> resp 404 "text/plain" "not found"

httpErr :: String -> ByteString
httpErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code httpCodes ("Unknown", "Unknown error")
                      body = BS.pack $ n ++ "\n\n" ++ d
                  in resp code "text/plain" body
  _ -> resp 400 "text/plain" "invalid code"

wsErr :: String -> ByteString
wsErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code wsCodes ("Unknown", "Unknown")
                  in resp 200 "application/json" $ json [("code", show code), ("name", n), ("description", d)]
  _ -> resp 400 "text/plain" "invalid code"

grpcErr :: String -> ByteString
grpcErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code grpcCodes ("UNKNOWN", "Unknown")
                  in resp 200 "application/json" $ json [("code", show code), ("status", n), ("message", d)]
  _ -> resp 400 "text/plain" "invalid code"

mcpErr :: String -> ByteString
mcpErr s = case reads s of
  [(code, "")] -> let (n, _) = lkp code mcpCodes ("Unknown", "Unknown")
                  in resp 200 "application/json" $ BS.pack $ "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":" ++ show code ++ ",\"message\":\"" ++ n ++ "\"},\"id\":null}"
  _ -> resp 400 "text/plain" "invalid code"

gqlErr :: String -> ByteString
gqlErr t = let (n, d) = lkpS t gqlCodes (t, "Unknown error")
           in resp 200 "application/json" $ BS.pack $ "{\"errors\":[{\"message\":\"" ++ d ++ "\",\"extensions\":{\"code\":\"" ++ n ++ "\"}}]}"

awsErr :: String -> ByteString
awsErr t = let (n, d, c) = lkpS t awsCodes (t, "Unknown error", 400)
           in resp c "application/json" $ BS.pack $ "{\"__type\":\"" ++ n ++ "\",\"message\":\"" ++ d ++ "\"}"

stripeErr :: String -> ByteString
stripeErr t = let (n, d, c) = lkpS t stripeCodes (t, "Unknown error", 400)
              in resp c "application/json" $ BS.pack $ "{\"error\":{\"type\":\"" ++ n ++ "\",\"message\":\"" ++ d ++ "\"}}"

fastapiErr :: String -> ByteString
fastapiErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code fastapiCodes ("Error", "Request error")
                  in resp code "application/json" $ BS.pack $ "{\"detail\":[{\"type\":\"" ++ n ++ "\",\"msg\":\"" ++ d ++ "\"}]}"
  _ -> resp 400 "application/json" "{\"detail\":\"invalid code\"}"

pydanticErr :: String -> ByteString
pydanticErr t = let (n, d) = lkpS t pydanticCodes (t, "Validation error")
                in resp 422 "application/json" $ BS.pack $ "{\"detail\":[{\"type\":\"" ++ n ++ "\",\"msg\":\"" ++ d ++ "\",\"loc\":[\"body\"]}]}"

oauthErr :: String -> ByteString
oauthErr t = let (n, d, c) = lkpS t oauthCodes (t, "OAuth error", 400)
             in resp c "application/json" $ BS.pack $ "{\"error\":\"" ++ n ++ "\",\"error_description\":\"" ++ d ++ "\"}"

pgErr :: String -> ByteString
pgErr t = let (n, d) = lkpS t pgCodes (t, "Database error")
          in resp 500 "application/json" $ BS.pack $ "{\"code\":\"" ++ t ++ "\",\"name\":\"" ++ n ++ "\",\"message\":\"" ++ d ++ "\"}"

mysqlErr :: String -> ByteString
mysqlErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code mysqlCodes ("Error", "Database error")
                  in resp 500 "application/json" $ BS.pack $ "{\"errno\":" ++ show code ++ ",\"sqlstate\":\"" ++ n ++ "\",\"message\":\"" ++ d ++ "\"}"
  _ -> resp 400 "text/plain" "invalid code"

redisErr :: String -> ByteString
redisErr t = let (n, d) = lkpS t redisCodes (t, "Redis error")
             in resp 500 "text/plain" $ BS.pack $ "-" ++ n ++ " " ++ d

mongoErr :: String -> ByteString
mongoErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code mongoCodes ("Error", "Database error")
                  in resp 500 "application/json" $ BS.pack $ "{\"ok\":0,\"code\":" ++ show code ++ ",\"codeName\":\"" ++ n ++ "\",\"errmsg\":\"" ++ d ++ "\"}"
  _ -> resp 400 "text/plain" "invalid code"

elasticErr :: String -> ByteString
elasticErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code elasticCodes ("error", "Elasticsearch error")
                  in resp code "application/json" $ BS.pack $ "{\"error\":{\"type\":\"" ++ n ++ "\",\"reason\":\"" ++ d ++ "\"},\"status\":" ++ show code ++ "}"
  _ -> resp 400 "text/plain" "invalid code"

k8sErr :: String -> ByteString
k8sErr t = let (n, d, c) = lkpS t k8sCodes (t, "Kubernetes error", 400)
           in resp c "application/json" $ BS.pack $ "{\"kind\":\"Status\",\"apiVersion\":\"v1\",\"status\":\"Failure\",\"message\":\"" ++ d ++ "\",\"reason\":\"" ++ n ++ "\",\"code\":" ++ show c ++ "}"

dockerErr :: String -> ByteString
dockerErr t = let (n, d, c) = lkpS t dockerCodes (t, "Docker error", 500)
              in resp c "application/json" $ BS.pack $ "{\"message\":\"" ++ d ++ "\",\"error\":\"" ++ n ++ "\"}"

firebaseErr :: String -> ByteString
firebaseErr t = let (n, d, c) = lkpS t firebaseCodes (t, "Firebase error", 400)
                in resp c "application/json" $ BS.pack $ "{\"error\":{\"code\":" ++ show c ++ ",\"message\":\"" ++ d ++ "\",\"status\":\"" ++ n ++ "\"}}"

twilioErr :: String -> ByteString
twilioErr s = case reads s of
  [(code, "")] -> let (n, d, c) = lkp code twilioCodes ("Error", "Twilio error", 400)
                  in resp c "application/json" $ BS.pack $ "{\"code\":" ++ show code ++ ",\"message\":\"" ++ n ++ "\",\"more_info\":\"" ++ d ++ "\"}"
  _ -> resp 400 "text/plain" "invalid code"

sendgridErr :: String -> ByteString
sendgridErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code sendgridCodes ("error", "SendGrid error")
                  in resp code "application/json" $ BS.pack $ "{\"errors\":[{\"message\":\"" ++ n ++ "\",\"field\":null,\"help\":\"" ++ d ++ "\"}]}"
  _ -> resp 400 "text/plain" "invalid code"

cloudflareErr :: String -> ByteString
cloudflareErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code cloudflareCodes ("Error", "Cloudflare error")
                  in resp code "text/html" $ BS.pack $ "<html><body><h1>" ++ show code ++ " " ++ n ++ "</h1><p>" ++ d ++ "</p></body></html>"
  _ -> resp 400 "text/plain" "invalid code"

vercelErr :: String -> ByteString
vercelErr t = let (n, d, c) = lkpS t vercelCodes (t, "Vercel error", 400)
              in resp c "application/json" $ BS.pack $ "{\"error\":{\"code\":\"" ++ n ++ "\",\"message\":\"" ++ d ++ "\"}}"

supabaseErr :: String -> ByteString
supabaseErr t = let (n, d, c) = lkpS t supabaseCodes (t, "Supabase error", 400)
                in resp c "application/json" $ BS.pack $ "{\"code\":\"" ++ n ++ "\",\"msg\":\"" ++ d ++ "\"}"

htcpcpErr :: String -> ByteString
htcpcpErr s = case reads s of
  [(code, "")] -> let (n, d) = lkp code htcpcpCodes ("Unknown", "Unknown HTCPCP error")
                      body = BS.pack $ n ++ "\n\n" ++ d
                  in resp code "message/coffeepot" body
  _ -> case lkpS s htcpcpStrCodes ("", "", 0) of
         ("", _, _) -> resp 400 "message/coffeepot" "invalid coffee request"
         (n, d, c) -> resp c "message/coffeepot" $ BS.pack $ n ++ "\n\n" ++ d

lkp :: Ord k => k -> Map k v -> v -> v
lkp k m d = Map.findWithDefault d k m

lkpS :: String -> Map String v -> v -> v
lkpS = lkp

json :: [(String, String)] -> ByteString
json pairs = BS.pack $ "{" ++ go pairs ++ "}"
  where go [] = ""
        go [(k,v)] = "\"" ++ k ++ "\":\"" ++ v ++ "\""
        go ((k,v):xs) = "\"" ++ k ++ "\":\"" ++ v ++ "\"," ++ go xs

resp :: Int -> ByteString -> ByteString -> ByteString
resp code contentType body = BS.concat
  [ "HTTP/1.1 ", BS.pack (show code), " ", statusPhrase code, "\r\n"
  , "Content-Type: ", contentType, "\r\n"
  , "Content-Length: ", BS.pack (show $ BS.length body), "\r\n"
  , "Connection: close\r\n"
  , "Access-Control-Allow-Origin: *\r\n"
  , "\r\n"
  , body
  ]

statusPhrase :: Int -> ByteString
statusPhrase c = maybe "Unknown" (BS.pack . fst) $ Map.lookup c httpCodes

--------------------------------------------------------------------------------
-- HTTP Status Codes (Complete)
--------------------------------------------------------------------------------
httpCodes :: Map Int (String, String)
httpCodes = Map.fromList
  -- 1xx Informational
  [ (100, ("Continue", "Server received request headers, client should proceed"))
  , (101, ("Switching Protocols", "Server is switching protocols as requested"))
  , (102, ("Processing", "Server is processing the request (WebDAV)"))
  , (103, ("Early Hints", "Preload resources while server prepares response"))
  -- 2xx Success
  , (200, ("OK", "Request succeeded"))
  , (201, ("Created", "Request succeeded and resource was created"))
  , (202, ("Accepted", "Request accepted for processing"))
  , (203, ("Non-Authoritative Information", "Response from transforming proxy"))
  , (204, ("No Content", "No content to return"))
  , (205, ("Reset Content", "Reset the document view"))
  , (206, ("Partial Content", "Partial resource returned (range request)"))
  , (207, ("Multi-Status", "Multiple status codes (WebDAV)"))
  , (208, ("Already Reported", "Members already enumerated (WebDAV)"))
  , (226, ("IM Used", "Response is result of instance-manipulations"))
  -- 3xx Redirection
  , (300, ("Multiple Choices", "Multiple options for the resource"))
  , (301, ("Moved Permanently", "Resource moved permanently"))
  , (302, ("Found", "Resource temporarily at different URI"))
  , (303, ("See Other", "Response at different URI via GET"))
  , (304, ("Not Modified", "Resource not modified since last request"))
  , (305, ("Use Proxy", "Must access through proxy (deprecated)"))
  , (306, ("Switch Proxy", "No longer used"))
  , (307, ("Temporary Redirect", "Temporary redirect, same method"))
  , (308, ("Permanent Redirect", "Permanent redirect, same method"))
  -- 4xx Client Errors
  , (400, ("Bad Request", "Malformed request syntax"))
  , (401, ("Unauthorized", "Authentication required"))
  , (402, ("Payment Required", "Payment required"))
  , (403, ("Forbidden", "Server refuses to authorize"))
  , (404, ("Not Found", "Resource not found"))
  , (405, ("Method Not Allowed", "Method not allowed for resource"))
  , (406, ("Not Acceptable", "No acceptable representation"))
  , (407, ("Proxy Authentication Required", "Proxy authentication required"))
  , (408, ("Request Timeout", "Server timed out waiting"))
  , (409, ("Conflict", "Request conflicts with server state"))
  , (410, ("Gone", "Resource no longer available"))
  , (411, ("Length Required", "Content-Length header required"))
  , (412, ("Precondition Failed", "Precondition in headers failed"))
  , (413, ("Payload Too Large", "Request payload too large"))
  , (414, ("URI Too Long", "URI too long"))
  , (415, ("Unsupported Media Type", "Media type not supported"))
  , (416, ("Range Not Satisfiable", "Range cannot be satisfied"))
  , (417, ("Expectation Failed", "Expect header cannot be met"))
  , (418, ("I'm a teapot", "Server is a teapot (RFC 2324)"))
  , (421, ("Misdirected Request", "Request to wrong server"))
  , (422, ("Unprocessable Entity", "Semantic errors in request"))
  , (423, ("Locked", "Resource is locked (WebDAV)"))
  , (424, ("Failed Dependency", "Dependency failed (WebDAV)"))
  , (425, ("Too Early", "Unwilling to process replay risk"))
  , (426, ("Upgrade Required", "Client should upgrade protocol"))
  , (428, ("Precondition Required", "Request must be conditional"))
  , (429, ("Too Many Requests", "Rate limit exceeded"))
  , (431, ("Request Header Fields Too Large", "Headers too large"))
  , (451, ("Unavailable For Legal Reasons", "Blocked for legal reasons"))
  -- 5xx Server Errors
  , (500, ("Internal Server Error", "Server encountered an error"))
  , (501, ("Not Implemented", "Functionality not implemented"))
  , (502, ("Bad Gateway", "Invalid upstream response"))
  , (503, ("Service Unavailable", "Service temporarily unavailable"))
  , (504, ("Gateway Timeout", "Upstream server timed out"))
  , (505, ("HTTP Version Not Supported", "HTTP version not supported"))
  , (506, ("Variant Also Negotiates", "Circular reference in negotiation"))
  , (507, ("Insufficient Storage", "Cannot store representation (WebDAV)"))
  , (508, ("Loop Detected", "Infinite loop detected (WebDAV)"))
  , (510, ("Not Extended", "Extensions required"))
  , (511, ("Network Authentication Required", "Network auth required"))
  -- Unofficial
  , (420, ("Enhance Your Calm", "Twitter rate limiting"))
  , (430, ("Request Header Fields Too Large", "Shopify"))
  , (440, ("Login Time-out", "IIS login timeout"))
  , (444, ("No Response", "Nginx closed without response"))
  , (449, ("Retry With", "Microsoft retry with action"))
  , (450, ("Blocked by Parental Controls", "Microsoft"))
  , (451, ("Unavailable For Legal Reasons", "Legal block"))
  , (460, ("Client Closed Request", "AWS ELB client closed"))
  , (463, ("Too Many IPs", "AWS ELB too many IPs"))
  , (494, ("Request Header Too Large", "Nginx"))
  , (495, ("SSL Certificate Error", "Nginx SSL error"))
  , (496, ("SSL Certificate Required", "Nginx SSL required"))
  , (497, ("HTTP Request Sent to HTTPS Port", "Nginx"))
  , (499, ("Client Closed Request", "Nginx client closed"))
  -- Cloudflare
  , (520, ("Web Server Returned Unknown Error", "Cloudflare unknown error"))
  , (521, ("Web Server Is Down", "Cloudflare origin down"))
  , (522, ("Connection Timed Out", "Cloudflare connection timeout"))
  , (523, ("Origin Is Unreachable", "Cloudflare origin unreachable"))
  , (524, ("A Timeout Occurred", "Cloudflare timeout"))
  , (525, ("SSL Handshake Failed", "Cloudflare SSL handshake failed"))
  , (526, ("Invalid SSL Certificate", "Cloudflare invalid cert"))
  , (527, ("Railgun Error", "Cloudflare Railgun error"))
  , (530, ("Origin DNS Error", "Cloudflare DNS error"))
  -- AWS
  , (561, ("Unauthorized", "AWS ELB unauthorized"))
  -- Network
  , (598, ("Network Read Timeout", "Proxy network read timeout"))
  , (599, ("Network Connect Timeout", "Proxy network connect timeout"))
  ]

--------------------------------------------------------------------------------
-- WebSocket Close Codes (Complete per RFC 6455 + Extensions)
--------------------------------------------------------------------------------
wsCodes :: Map Int (String, String)
wsCodes = Map.fromList
  [ (1000, ("Normal Closure", "Connection completed purpose"))
  , (1001, ("Going Away", "Endpoint going away"))
  , (1002, ("Protocol Error", "Protocol error"))
  , (1003, ("Unsupported Data", "Unsupported data type"))
  , (1004, ("Reserved", "Reserved for future use"))
  , (1005, ("No Status Received", "No status code in close frame"))
  , (1006, ("Abnormal Closure", "Connection closed abnormally"))
  , (1007, ("Invalid Payload Data", "Inconsistent message data type"))
  , (1008, ("Policy Violation", "Message violates policy"))
  , (1009, ("Message Too Big", "Message too large"))
  , (1010, ("Mandatory Extension", "Server didn't negotiate extension"))
  , (1011, ("Internal Error", "Unexpected server condition"))
  , (1012, ("Service Restart", "Server restarting"))
  , (1013, ("Try Again Later", "Temporary overload"))
  , (1014, ("Bad Gateway", "Gateway received invalid response"))
  , (1015, ("TLS Handshake", "TLS handshake failure"))
  -- Registered (IANA)
  , (3000, ("Unauthorized", "Application unauthorized"))
  , (3001, ("Forbidden", "Application forbidden"))
  , (3002, ("Not Found", "Application not found"))
  , (3003, ("Timeout", "Application timeout"))
  , (3004, ("Rate Limited", "Application rate limited"))
  , (3005, ("Invalid Message", "Invalid message format"))
  , (3006, ("Session Expired", "Session expired"))
  , (3007, ("Server Error", "Server error"))
  , (3008, ("Reconnect", "Client should reconnect"))
  -- Private Use (4000-4999)
  , (4000, ("Unknown Error", "Unknown application error"))
  , (4001, ("Invalid Opcode", "Unknown opcode"))
  , (4002, ("Decode Error", "Failed to decode message"))
  , (4003, ("Not Authenticated", "Not authenticated"))
  , (4004, ("Authentication Failed", "Authentication failed"))
  , (4005, ("Already Authenticated", "Already authenticated"))
  , (4006, ("Session Invalid", "Invalid session"))
  , (4007, ("Session Timeout", "Session timed out"))
  , (4008, ("Server Full", "Server at capacity"))
  , (4009, ("Rate Limited", "Too many messages"))
  , (4010, ("Invalid Payload", "Invalid payload"))
  , (4011, ("Server Shutdown", "Server shutting down"))
  , (4012, ("Invalid Version", "Protocol version mismatch"))
  , (4013, ("Invalid Intent", "Invalid intent"))
  , (4014, ("Disallowed Intent", "Disallowed intent"))
  , (4015, ("Sharding Required", "Sharding required"))
  , (4016, ("Invalid Shard", "Invalid shard"))
  -- Discord specific
  , (4100, ("Voice Unknown Opcode", "Discord voice unknown opcode"))
  , (4101, ("Voice Decode Error", "Discord voice decode error"))
  , (4102, ("Voice Not Authenticated", "Discord voice not authenticated"))
  , (4103, ("Voice Authentication Failed", "Discord voice auth failed"))
  , (4104, ("Voice Already Authenticated", "Discord voice already auth"))
  , (4105, ("Voice Invalid Session", "Discord voice invalid session"))
  , (4106, ("Voice Session Timeout", "Discord voice session timeout"))
  , (4107, ("Voice Server Not Found", "Discord voice server not found"))
  , (4108, ("Voice Unknown Protocol", "Discord voice unknown protocol"))
  , (4109, ("Voice Disconnected", "Discord voice disconnected"))
  , (4110, ("Voice Server Crashed", "Discord voice server crashed"))
  , (4111, ("Voice Unknown Encryption", "Discord voice unknown encryption"))
  ]

--------------------------------------------------------------------------------
-- gRPC Status Codes (Complete)
--------------------------------------------------------------------------------
grpcCodes :: Map Int (String, String)
grpcCodes = Map.fromList
  [ (0, ("OK", "Success"))
  , (1, ("CANCELLED", "Operation cancelled by caller"))
  , (2, ("UNKNOWN", "Unknown error"))
  , (3, ("INVALID_ARGUMENT", "Invalid argument provided"))
  , (4, ("DEADLINE_EXCEEDED", "Deadline expired"))
  , (5, ("NOT_FOUND", "Resource not found"))
  , (6, ("ALREADY_EXISTS", "Resource already exists"))
  , (7, ("PERMISSION_DENIED", "Permission denied"))
  , (8, ("RESOURCE_EXHAUSTED", "Resource exhausted (quota/memory)"))
  , (9, ("FAILED_PRECONDITION", "Operation rejected, system not in required state"))
  , (10, ("ABORTED", "Operation aborted due to concurrency issue"))
  , (11, ("OUT_OF_RANGE", "Operation attempted past valid range"))
  , (12, ("UNIMPLEMENTED", "Operation not implemented"))
  , (13, ("INTERNAL", "Internal server error"))
  , (14, ("UNAVAILABLE", "Service unavailable"))
  , (15, ("DATA_LOSS", "Unrecoverable data loss"))
  , (16, ("UNAUTHENTICATED", "Request not authenticated"))
  ]

--------------------------------------------------------------------------------
-- MCP / JSON-RPC Errors (Complete)
--------------------------------------------------------------------------------
mcpCodes :: Map Int (String, String)
mcpCodes = Map.fromList
  -- JSON-RPC 2.0 Standard
  [ (-32700, ("Parse error", "Invalid JSON"))
  , (-32600, ("Invalid Request", "Invalid request object"))
  , (-32601, ("Method not found", "Method does not exist"))
  , (-32602, ("Invalid params", "Invalid method parameters"))
  , (-32603, ("Internal error", "Internal JSON-RPC error"))
  -- Server Errors (-32000 to -32099)
  , (-32000, ("Server error", "Generic server error"))
  , (-32001, ("Resource not found", "MCP resource not found"))
  , (-32002, ("Resource unavailable", "MCP resource unavailable"))
  , (-32003, ("Tool not found", "MCP tool not found"))
  , (-32004, ("Tool execution failed", "MCP tool execution error"))
  , (-32005, ("Prompt not found", "MCP prompt not found"))
  , (-32006, ("Invalid resource URI", "Invalid resource URI format"))
  , (-32007, ("Permission denied", "Insufficient permissions"))
  , (-32008, ("Rate limited", "Too many requests"))
  , (-32009, ("Context too large", "Context exceeds limit"))
  , (-32010, ("Capability not supported", "Capability not available"))
  , (-32011, ("Protocol version mismatch", "Incompatible protocol version"))
  , (-32012, ("Session expired", "MCP session expired"))
  , (-32013, ("Initialization failed", "Server init failed"))
  , (-32014, ("Shutdown in progress", "Server shutting down"))
  , (-32015, ("Sampling not allowed", "Sampling not permitted"))
  , (-32016, ("Roots changed", "Roots list changed"))
  , (-32017, ("Content too large", "Content exceeds size limit"))
  , (-32018, ("Invalid cursor", "Invalid pagination cursor"))
  , (-32019, ("Subscription failed", "Failed to subscribe"))
  , (-32020, ("Notification failed", "Failed to send notification"))
  -- Extended errors
  , (-32050, ("Transport error", "Transport layer error"))
  , (-32051, ("Connection lost", "Connection to server lost"))
  , (-32052, ("Timeout", "Request timed out"))
  , (-32053, ("Cancelled", "Request cancelled"))
  , (-32099, ("Server not initialized", "Server not initialized"))
  ]

--------------------------------------------------------------------------------
-- GraphQL Errors (Complete)
--------------------------------------------------------------------------------
gqlCodes :: Map String (String, String)
gqlCodes = Map.fromList
  -- Apollo Standard
  [ ("GRAPHQL_PARSE_FAILED", ("GRAPHQL_PARSE_FAILED", "Syntax error in GraphQL query"))
  , ("GRAPHQL_VALIDATION_FAILED", ("GRAPHQL_VALIDATION_FAILED", "Query violates schema"))
  , ("BAD_USER_INPUT", ("BAD_USER_INPUT", "Invalid argument value"))
  , ("UNAUTHENTICATED", ("UNAUTHENTICATED", "Not authenticated"))
  , ("FORBIDDEN", ("FORBIDDEN", "Not authorized"))
  , ("PERSISTED_QUERY_NOT_FOUND", ("PERSISTED_QUERY_NOT_FOUND", "Persisted query not found"))
  , ("PERSISTED_QUERY_NOT_SUPPORTED", ("PERSISTED_QUERY_NOT_SUPPORTED", "Persisted queries disabled"))
  , ("OPERATION_RESOLUTION_FAILURE", ("OPERATION_RESOLUTION_FAILURE", "Cannot resolve operation"))
  , ("BAD_REQUEST", ("BAD_REQUEST", "Invalid request"))
  , ("INTERNAL_SERVER_ERROR", ("INTERNAL_SERVER_ERROR", "Internal error"))
  -- Extended
  , ("QUERY_TOO_COMPLEX", ("QUERY_TOO_COMPLEX", "Query exceeds complexity limit"))
  , ("QUERY_TOO_DEEP", ("QUERY_TOO_DEEP", "Query exceeds depth limit"))
  , ("RATE_LIMITED", ("RATE_LIMITED", "Rate limit exceeded"))
  , ("FIELD_NOT_FOUND", ("FIELD_NOT_FOUND", "Field does not exist"))
  , ("TYPE_NOT_FOUND", ("TYPE_NOT_FOUND", "Type does not exist"))
  , ("VARIABLE_NOT_PROVIDED", ("VARIABLE_NOT_PROVIDED", "Required variable missing"))
  , ("VARIABLE_TYPE_MISMATCH", ("VARIABLE_TYPE_MISMATCH", "Variable type mismatch"))
  , ("DIRECTIVE_NOT_FOUND", ("DIRECTIVE_NOT_FOUND", "Directive not found"))
  , ("INVALID_DIRECTIVE_LOCATION", ("INVALID_DIRECTIVE_LOCATION", "Invalid directive location"))
  , ("SUBSCRIPTION_FAILED", ("SUBSCRIPTION_FAILED", "Subscription failed"))
  , ("SUBSCRIPTION_TERMINATED", ("SUBSCRIPTION_TERMINATED", "Subscription terminated"))
  , ("DOWNSTREAM_SERVICE_ERROR", ("DOWNSTREAM_SERVICE_ERROR", "Downstream service error"))
  ]

--------------------------------------------------------------------------------
-- AWS Errors (Comprehensive)
--------------------------------------------------------------------------------
awsCodes :: Map String (String, String, Int)
awsCodes = Map.fromList
  -- Common
  [ ("AccessDenied", ("AccessDeniedException", "Access denied", 403))
  , ("AccessDeniedException", ("AccessDeniedException", "Access denied", 403))
  , ("ExpiredToken", ("ExpiredTokenException", "Security token expired", 403))
  , ("ExpiredTokenException", ("ExpiredTokenException", "Security token expired", 403))
  , ("IncompleteSignature", ("IncompleteSignature", "Invalid request signature", 403))
  , ("InternalFailure", ("InternalFailure", "Internal service error", 500))
  , ("InternalError", ("InternalError", "Internal error", 500))
  , ("InvalidAction", ("InvalidAction", "Invalid action", 400))
  , ("InvalidClientTokenId", ("InvalidClientTokenId", "Invalid access key", 403))
  , ("InvalidParameterCombination", ("InvalidParameterCombination", "Invalid parameter combination", 400))
  , ("InvalidParameterValue", ("InvalidParameterValue", "Invalid parameter value", 400))
  , ("InvalidQueryParameter", ("InvalidQueryParameter", "Malformed query string", 400))
  , ("MalformedQueryString", ("MalformedQueryString", "Query string syntax error", 400))
  , ("MissingAction", ("MissingAction", "Missing action parameter", 400))
  , ("MissingAuthenticationToken", ("MissingAuthenticationToken", "Missing auth token", 403))
  , ("MissingParameter", ("MissingParameter", "Required parameter missing", 400))
  , ("NotAuthorized", ("NotAuthorized", "Not authorized", 403))
  , ("OptInRequired", ("OptInRequired", "Subscription required", 403))
  , ("RequestExpired", ("RequestExpired", "Request timestamp expired", 400))
  , ("ServiceUnavailable", ("ServiceUnavailable", "Service unavailable", 503))
  , ("ThrottlingException", ("ThrottlingException", "Request throttled", 429))
  , ("Throttling", ("Throttling", "Request throttled", 429))
  , ("ValidationError", ("ValidationError", "Validation failed", 400))
  , ("ValidationException", ("ValidationException", "Validation failed", 400))
  -- Resource
  , ("ResourceNotFound", ("ResourceNotFoundException", "Resource not found", 404))
  , ("ResourceNotFoundException", ("ResourceNotFoundException", "Resource not found", 404))
  , ("ResourceInUse", ("ResourceInUseException", "Resource in use", 409))
  , ("ResourceAlreadyExists", ("ResourceAlreadyExistsException", "Resource exists", 409))
  -- Limits
  , ("LimitExceeded", ("LimitExceededException", "Limit exceeded", 400))
  , ("LimitExceededException", ("LimitExceededException", "Limit exceeded", 400))
  , ("TooManyRequests", ("TooManyRequestsException", "Too many requests", 429))
  , ("ProvisionedThroughputExceeded", ("ProvisionedThroughputExceededException", "Throughput exceeded", 400))
  -- S3 specific
  , ("NoSuchBucket", ("NoSuchBucket", "Bucket does not exist", 404))
  , ("NoSuchKey", ("NoSuchKey", "Object does not exist", 404))
  , ("BucketAlreadyExists", ("BucketAlreadyExists", "Bucket already exists", 409))
  , ("BucketNotEmpty", ("BucketNotEmpty", "Bucket not empty", 409))
  -- DynamoDB
  , ("ConditionalCheckFailed", ("ConditionalCheckFailedException", "Condition check failed", 400))
  , ("ItemCollectionSizeLimitExceeded", ("ItemCollectionSizeLimitExceededException", "Item collection too large", 400))
  , ("TransactionConflict", ("TransactionConflictException", "Transaction conflict", 409))
  -- Lambda
  , ("CodeStorageExceeded", ("CodeStorageExceededException", "Code storage limit exceeded", 400))
  , ("InvalidRuntimeException", ("InvalidRuntimeException", "Invalid runtime", 400))
  , ("ResourceConflict", ("ResourceConflictException", "Resource conflict", 409))
  -- Cognito
  , ("UserNotFound", ("UserNotFoundException", "User not found", 404))
  , ("UsernameExists", ("UsernameExistsException", "Username exists", 400))
  , ("NotAuthorizedError", ("NotAuthorizedException", "Not authorized", 403))
  , ("CodeMismatch", ("CodeMismatchException", "Invalid verification code", 400))
  , ("ExpiredCode", ("ExpiredCodeException", "Verification code expired", 400))
  ]

--------------------------------------------------------------------------------
-- Stripe Errors (Complete)
--------------------------------------------------------------------------------
stripeCodes :: Map String (String, String, Int)
stripeCodes = Map.fromList
  [ ("api_error", ("api_error", "Internal API error", 500))
  , ("authentication_error", ("authentication_error", "Invalid API key", 401))
  , ("card_error", ("card_error", "Card error", 402))
  , ("idempotency_error", ("idempotency_error", "Idempotency key conflict", 400))
  , ("invalid_request_error", ("invalid_request_error", "Invalid parameters", 400))
  , ("rate_limit_error", ("rate_limit_error", "Too many requests", 429))
  , ("validation_error", ("validation_error", "Validation error", 400))
  -- Card decline codes
  , ("card_declined", ("card_error", "Card was declined", 402))
  , ("expired_card", ("card_error", "Card has expired", 402))
  , ("incorrect_cvc", ("card_error", "Incorrect CVC", 402))
  , ("incorrect_number", ("card_error", "Incorrect card number", 402))
  , ("incorrect_zip", ("card_error", "Incorrect ZIP code", 402))
  , ("insufficient_funds", ("card_error", "Insufficient funds", 402))
  , ("invalid_cvc", ("card_error", "Invalid CVC", 402))
  , ("invalid_expiry_month", ("card_error", "Invalid expiration month", 402))
  , ("invalid_expiry_year", ("card_error", "Invalid expiration year", 402))
  , ("invalid_number", ("card_error", "Invalid card number", 402))
  , ("processing_error", ("card_error", "Processing error", 402))
  , ("card_not_supported", ("card_error", "Card not supported", 402))
  , ("currency_not_supported", ("card_error", "Currency not supported", 402))
  , ("duplicate_transaction", ("card_error", "Duplicate transaction", 402))
  , ("fraudulent", ("card_error", "Suspected fraud", 402))
  , ("generic_decline", ("card_error", "Card declined", 402))
  , ("lost_card", ("card_error", "Lost card", 402))
  , ("stolen_card", ("card_error", "Stolen card", 402))
  , ("merchant_blacklist", ("card_error", "Merchant blacklisted", 402))
  , ("pickup_card", ("card_error", "Card flagged for pickup", 402))
  , ("restricted_card", ("card_error", "Card restricted", 402))
  , ("revocation_of_authorization", ("card_error", "Authorization revoked", 402))
  , ("security_violation", ("card_error", "Security violation", 402))
  , ("service_not_allowed", ("card_error", "Service not allowed", 402))
  , ("withdrawal_count_limit_exceeded", ("card_error", "Withdrawal limit exceeded", 402))
  ]

--------------------------------------------------------------------------------
-- FastAPI/Starlette Errors
--------------------------------------------------------------------------------
fastapiCodes :: Map Int (String, String)
fastapiCodes = Map.fromList
  [ (400, ("bad_request", "Bad request"))
  , (401, ("unauthorized", "Not authenticated"))
  , (403, ("forbidden", "Not authorized"))
  , (404, ("not_found", "Resource not found"))
  , (405, ("method_not_allowed", "Method not allowed"))
  , (409, ("conflict", "Resource conflict"))
  , (422, ("unprocessable_entity", "Validation error"))
  , (429, ("too_many_requests", "Rate limit exceeded"))
  , (500, ("internal_server_error", "Internal server error"))
  , (502, ("bad_gateway", "Bad gateway"))
  , (503, ("service_unavailable", "Service unavailable"))
  , (504, ("gateway_timeout", "Gateway timeout"))
  ]

--------------------------------------------------------------------------------
-- Pydantic Validation Errors
--------------------------------------------------------------------------------
pydanticCodes :: Map String (String, String)
pydanticCodes = Map.fromList
  [ ("value_error", ("value_error", "Invalid value"))
  , ("type_error", ("type_error", "Invalid type"))
  , ("missing", ("value_error.missing", "Field required"))
  , ("string_type", ("type_error.string", "String required"))
  , ("int_type", ("type_error.integer", "Integer required"))
  , ("float_type", ("type_error.float", "Number required"))
  , ("bool_type", ("type_error.bool", "Boolean required"))
  , ("list_type", ("type_error.list", "List required"))
  , ("dict_type", ("type_error.dict", "Dictionary required"))
  , ("string_too_short", ("value_error.any_str.min_length", "String too short"))
  , ("string_too_long", ("value_error.any_str.max_length", "String too long"))
  , ("string_pattern", ("value_error.str.regex", "String does not match pattern"))
  , ("int_too_small", ("value_error.number.not_ge", "Number too small"))
  , ("int_too_large", ("value_error.number.not_le", "Number too large"))
  , ("email", ("value_error.email", "Invalid email"))
  , ("url", ("value_error.url", "Invalid URL"))
  , ("uuid", ("value_error.uuid", "Invalid UUID"))
  , ("datetime", ("value_error.datetime", "Invalid datetime"))
  , ("date", ("value_error.date", "Invalid date"))
  , ("time", ("value_error.time", "Invalid time"))
  , ("json", ("value_error.json", "Invalid JSON"))
  , ("enum", ("type_error.enum", "Invalid enum value"))
  , ("extra_forbidden", ("value_error.extra", "Extra fields not permitted"))
  ]

--------------------------------------------------------------------------------
-- OAuth 2.0 Errors (RFC 6749)
--------------------------------------------------------------------------------
oauthCodes :: Map String (String, String, Int)
oauthCodes = Map.fromList
  [ ("invalid_request", ("invalid_request", "Request missing required parameter", 400))
  , ("invalid_client", ("invalid_client", "Client authentication failed", 401))
  , ("invalid_grant", ("invalid_grant", "Invalid authorization grant", 400))
  , ("invalid_scope", ("invalid_scope", "Invalid scope requested", 400))
  , ("invalid_token", ("invalid_token", "Token is invalid or expired", 401))
  , ("insufficient_scope", ("insufficient_scope", "Token has insufficient scope", 403))
  , ("unauthorized_client", ("unauthorized_client", "Client not authorized", 401))
  , ("unsupported_grant_type", ("unsupported_grant_type", "Grant type not supported", 400))
  , ("unsupported_response_type", ("unsupported_response_type", "Response type not supported", 400))
  , ("unsupported_token_type", ("unsupported_token_type", "Token type not supported", 400))
  , ("access_denied", ("access_denied", "Resource owner denied request", 403))
  , ("server_error", ("server_error", "Authorization server error", 500))
  , ("temporarily_unavailable", ("temporarily_unavailable", "Server temporarily unavailable", 503))
  , ("login_required", ("login_required", "User must authenticate", 401))
  , ("consent_required", ("consent_required", "User consent required", 403))
  , ("interaction_required", ("interaction_required", "User interaction required", 400))
  , ("account_selection_required", ("account_selection_required", "Account selection required", 400))
  ]

--------------------------------------------------------------------------------
-- PostgreSQL Errors (SQLSTATE codes)
--------------------------------------------------------------------------------
pgCodes :: Map String (String, String)
pgCodes = Map.fromList
  -- Class 00 - Successful Completion
  [ ("00000", ("successful_completion", "Successful completion"))
  -- Class 01 - Warning
  , ("01000", ("warning", "Warning"))
  , ("0100C", ("dynamic_result_sets_returned", "Dynamic result sets returned"))
  -- Class 02 - No Data
  , ("02000", ("no_data", "No data found"))
  , ("02001", ("no_additional_dynamic_result_sets_returned", "No additional dynamic result sets"))
  -- Class 03 - SQL Statement Not Yet Complete
  , ("03000", ("sql_statement_not_yet_complete", "SQL statement not yet complete"))
  -- Class 08 - Connection Exception
  , ("08000", ("connection_exception", "Connection exception"))
  , ("08003", ("connection_does_not_exist", "Connection does not exist"))
  , ("08006", ("connection_failure", "Connection failure"))
  , ("08001", ("sqlclient_unable_to_establish_sqlconnection", "Unable to establish connection"))
  , ("08004", ("sqlserver_rejected_establishment_of_sqlconnection", "Server rejected connection"))
  -- Class 09 - Triggered Action Exception
  , ("09000", ("triggered_action_exception", "Triggered action exception"))
  -- Class 0A - Feature Not Supported
  , ("0A000", ("feature_not_supported", "Feature not supported"))
  -- Class 22 - Data Exception
  , ("22000", ("data_exception", "Data exception"))
  , ("22001", ("string_data_right_truncation", "String data truncated"))
  , ("22002", ("null_value_no_indicator_parameter", "Null value without indicator"))
  , ("22003", ("numeric_value_out_of_range", "Numeric value out of range"))
  , ("22004", ("null_value_not_allowed", "Null value not allowed"))
  , ("22007", ("invalid_datetime_format", "Invalid datetime format"))
  , ("22008", ("datetime_field_overflow", "Datetime field overflow"))
  , ("22012", ("division_by_zero", "Division by zero"))
  , ("22019", ("invalid_escape_character", "Invalid escape character"))
  , ("22021", ("character_not_in_repertoire", "Character not in repertoire"))
  , ("22023", ("invalid_parameter_value", "Invalid parameter value"))
  , ("22025", ("invalid_escape_sequence", "Invalid escape sequence"))
  , ("22026", ("string_data_length_mismatch", "String data length mismatch"))
  , ("22P02", ("invalid_text_representation", "Invalid text representation"))
  , ("22P03", ("invalid_binary_representation", "Invalid binary representation"))
  -- Class 23 - Integrity Constraint Violation
  , ("23000", ("integrity_constraint_violation", "Integrity constraint violation"))
  , ("23001", ("restrict_violation", "Restrict violation"))
  , ("23502", ("not_null_violation", "Not null violation"))
  , ("23503", ("foreign_key_violation", "Foreign key violation"))
  , ("23505", ("unique_violation", "Unique violation"))
  , ("23514", ("check_violation", "Check constraint violation"))
  , ("23P01", ("exclusion_violation", "Exclusion violation"))
  -- Class 25 - Invalid Transaction State
  , ("25000", ("invalid_transaction_state", "Invalid transaction state"))
  , ("25001", ("active_sql_transaction", "Active SQL transaction"))
  , ("25002", ("branch_transaction_already_active", "Branch transaction already active"))
  , ("25006", ("read_only_sql_transaction", "Read only SQL transaction"))
  , ("25007", ("schema_and_data_statement_mixing_not_supported", "Schema and data statement mixing not supported"))
  , ("25P01", ("no_active_sql_transaction", "No active SQL transaction"))
  , ("25P02", ("in_failed_sql_transaction", "In failed SQL transaction"))
  -- Class 28 - Invalid Authorization Specification
  , ("28000", ("invalid_authorization_specification", "Invalid authorization"))
  , ("28P01", ("invalid_password", "Invalid password"))
  -- Class 3D - Invalid Catalog Name
  , ("3D000", ("invalid_catalog_name", "Invalid catalog name"))
  -- Class 3F - Invalid Schema Name
  , ("3F000", ("invalid_schema_name", "Invalid schema name"))
  -- Class 40 - Transaction Rollback
  , ("40000", ("transaction_rollback", "Transaction rollback"))
  , ("40001", ("serialization_failure", "Serialization failure"))
  , ("40002", ("transaction_integrity_constraint_violation", "Transaction integrity constraint violation"))
  , ("40003", ("statement_completion_unknown", "Statement completion unknown"))
  , ("40P01", ("deadlock_detected", "Deadlock detected"))
  -- Class 42 - Syntax Error or Access Rule Violation
  , ("42000", ("syntax_error_or_access_rule_violation", "Syntax or access violation"))
  , ("42501", ("insufficient_privilege", "Insufficient privilege"))
  , ("42601", ("syntax_error", "Syntax error"))
  , ("42602", ("invalid_name", "Invalid name"))
  , ("42611", ("invalid_column_definition", "Invalid column definition"))
  , ("42622", ("name_too_long", "Name too long"))
  , ("42701", ("duplicate_column", "Duplicate column"))
  , ("42702", ("ambiguous_column", "Ambiguous column"))
  , ("42703", ("undefined_column", "Undefined column"))
  , ("42704", ("undefined_object", "Undefined object"))
  , ("42710", ("duplicate_object", "Duplicate object"))
  , ("42712", ("duplicate_alias", "Duplicate alias"))
  , ("42723", ("duplicate_function", "Duplicate function"))
  , ("42725", ("ambiguous_function", "Ambiguous function"))
  , ("42803", ("grouping_error", "Grouping error"))
  , ("42804", ("datatype_mismatch", "Datatype mismatch"))
  , ("42809", ("wrong_object_type", "Wrong object type"))
  , ("42830", ("invalid_foreign_key", "Invalid foreign key"))
  , ("42846", ("cannot_coerce", "Cannot coerce"))
  , ("42883", ("undefined_function", "Undefined function"))
  , ("42939", ("reserved_name", "Reserved name"))
  , ("42P01", ("undefined_table", "Undefined table"))
  , ("42P02", ("undefined_parameter", "Undefined parameter"))
  , ("42P03", ("duplicate_cursor", "Duplicate cursor"))
  , ("42P04", ("duplicate_database", "Duplicate database"))
  , ("42P05", ("duplicate_prepared_statement", "Duplicate prepared statement"))
  , ("42P06", ("duplicate_schema", "Duplicate schema"))
  , ("42P07", ("duplicate_table", "Duplicate table"))
  , ("42P08", ("ambiguous_parameter", "Ambiguous parameter"))
  , ("42P09", ("ambiguous_alias", "Ambiguous alias"))
  , ("42P10", ("invalid_column_reference", "Invalid column reference"))
  , ("42P11", ("invalid_cursor_definition", "Invalid cursor definition"))
  , ("42P12", ("invalid_database_definition", "Invalid database definition"))
  , ("42P13", ("invalid_function_definition", "Invalid function definition"))
  , ("42P14", ("invalid_prepared_statement_definition", "Invalid prepared statement definition"))
  , ("42P15", ("invalid_schema_definition", "Invalid schema definition"))
  , ("42P16", ("invalid_table_definition", "Invalid table definition"))
  , ("42P17", ("invalid_object_definition", "Invalid object definition"))
  , ("42P18", ("indeterminate_datatype", "Indeterminate datatype"))
  -- Class 53 - Insufficient Resources
  , ("53000", ("insufficient_resources", "Insufficient resources"))
  , ("53100", ("disk_full", "Disk full"))
  , ("53200", ("out_of_memory", "Out of memory"))
  , ("53300", ("too_many_connections", "Too many connections"))
  -- Class 54 - Program Limit Exceeded
  , ("54000", ("program_limit_exceeded", "Program limit exceeded"))
  , ("54001", ("statement_too_complex", "Statement too complex"))
  , ("54011", ("too_many_columns", "Too many columns"))
  , ("54023", ("too_many_arguments", "Too many arguments"))
  -- Class 57 - Operator Intervention
  , ("57000", ("operator_intervention", "Operator intervention"))
  , ("57014", ("query_canceled", "Query canceled"))
  , ("57P01", ("admin_shutdown", "Admin shutdown"))
  , ("57P02", ("crash_shutdown", "Crash shutdown"))
  , ("57P03", ("cannot_connect_now", "Cannot connect now"))
  -- Class 58 - System Error
  , ("58000", ("system_error", "System error"))
  , ("58030", ("io_error", "I/O error"))
  ]

--------------------------------------------------------------------------------
-- MySQL Errors
--------------------------------------------------------------------------------
mysqlCodes :: Map Int (String, String)
mysqlCodes = Map.fromList
  [ (1004, ("HY000", "Cannot create file"))
  , (1005, ("HY000", "Cannot create table"))
  , (1006, ("HY000", "Cannot create database"))
  , (1007, ("HY000", "Database already exists"))
  , (1008, ("HY000", "Cannot drop database"))
  , (1020, ("HY000", "Record file is full"))
  , (1021, ("HY000", "Disk full"))
  , (1022, ("23000", "Duplicate key"))
  , (1025, ("HY000", "Error on rename"))
  , (1040, ("08004", "Too many connections"))
  , (1041, ("HY000", "Out of memory"))
  , (1042, ("08S01", "Cannot get hostname"))
  , (1043, ("08S01", "Bad handshake"))
  , (1044, ("42000", "Access denied to database"))
  , (1045, ("28000", "Access denied for user"))
  , (1046, ("3D000", "No database selected"))
  , (1048, ("23000", "Column cannot be null"))
  , (1049, ("42000", "Unknown database"))
  , (1050, ("42S01", "Table already exists"))
  , (1051, ("42S02", "Unknown table"))
  , (1052, ("23000", "Ambiguous column"))
  , (1054, ("42S22", "Unknown column"))
  , (1060, ("42S21", "Duplicate column name"))
  , (1061, ("42000", "Duplicate key name"))
  , (1062, ("23000", "Duplicate entry"))
  , (1064, ("42000", "Syntax error"))
  , (1065, ("42000", "Query was empty"))
  , (1066, ("42000", "Not unique table/alias"))
  , (1067, ("42000", "Invalid default value"))
  , (1068, ("42000", "Multiple primary key"))
  , (1069, ("42000", "Too many keys"))
  , (1070, ("42000", "Too many key parts"))
  , (1071, ("42000", "Key too long"))
  , (1072, ("42000", "Key column doesn't exist"))
  , (1100, ("HY000", "Table not locked"))
  , (1142, ("42000", "Command denied"))
  , (1146, ("42S02", "Table doesn't exist"))
  , (1149, ("42000", "Syntax error"))
  , (1169, ("23000", "Unique constraint violation"))
  , (1205, ("HY000", "Lock wait timeout"))
  , (1213, ("40001", "Deadlock found"))
  , (1216, ("23000", "Foreign key constraint fails (parent)"))
  , (1217, ("23000", "Foreign key constraint fails (child)"))
  , (1227, ("42000", "Access denied"))
  , (1251, ("08004", "Client does not support authentication"))
  , (1290, ("HY000", "Server is running with read-only"))
  , (1317, ("70100", "Query execution was interrupted"))
  , (1451, ("23000", "Cannot delete parent row"))
  , (1452, ("23000", "Cannot add child row"))
  , (1644, ("45000", "User-defined exception"))
  , (1698, ("28000", "Access denied for user"))
  , (2002, ("HY000", "Cannot connect to server"))
  , (2003, ("HY000", "Cannot connect to MySQL server"))
  , (2005, ("HY000", "Unknown MySQL server host"))
  , (2006, ("HY000", "MySQL server has gone away"))
  , (2013, ("HY000", "Lost connection to server"))
  ]

--------------------------------------------------------------------------------
-- Redis Errors
--------------------------------------------------------------------------------
redisCodes :: Map String (String, String)
redisCodes = Map.fromList
  [ ("ERR", ("ERR", "Generic error"))
  , ("WRONGTYPE", ("WRONGTYPE", "Operation against wrong type"))
  , ("MOVED", ("MOVED", "Key moved to different slot"))
  , ("ASK", ("ASK", "Key temporarily in another slot"))
  , ("CLUSTERDOWN", ("CLUSTERDOWN", "Cluster is down"))
  , ("CROSSSLOT", ("CROSSSLOT", "Keys in different slots"))
  , ("TRYAGAIN", ("TRYAGAIN", "Try again later"))
  , ("NOSCRIPT", ("NOSCRIPT", "Script not found"))
  , ("LOADING", ("LOADING", "Redis is loading dataset"))
  , ("BUSY", ("BUSY", "Redis is busy with background task"))
  , ("READONLY", ("READONLY", "Cannot write to replica"))
  , ("NOAUTH", ("NOAUTH", "Authentication required"))
  , ("OOM", ("OOM", "Out of memory"))
  , ("EXECABORT", ("EXECABORT", "Transaction aborted"))
  , ("MASTERDOWN", ("MASTERDOWN", "Master is down"))
  , ("NOREPLICAS", ("NOREPLICAS", "Not enough replicas"))
  , ("MISCONF", ("MISCONF", "Server misconfigured"))
  , ("NOTBUSY", ("NOTBUSY", "No script in execution"))
  , ("NOPROTO", ("NOPROTO", "Protocol version not supported"))
  , ("NOPERM", ("NOPERM", "No permission for command"))
  , ("UNKILLABLE", ("UNKILLABLE", "Cannot kill client"))
  ]

--------------------------------------------------------------------------------
-- MongoDB Errors
--------------------------------------------------------------------------------
mongoCodes :: Map Int (String, String)
mongoCodes = Map.fromList
  [ (1, ("InternalError", "Internal error"))
  , (2, ("BadValue", "Bad value"))
  , (3, ("NoSuchKey", "No such key"))
  , (4, ("GraphContainsCycle", "Graph contains cycle"))
  , (5, ("HostUnreachable", "Host unreachable"))
  , (6, ("HostNotFound", "Host not found"))
  , (7, ("UnknownError", "Unknown error"))
  , (8, ("FailedToParse", "Failed to parse"))
  , (9, ("CannotMutateObject", "Cannot mutate object"))
  , (10, ("UserNotFound", "User not found"))
  , (11, ("UnsupportedFormat", "Unsupported format"))
  , (12, ("Unauthorized", "Unauthorized"))
  , (13, ("TypeMismatch", "Type mismatch"))
  , (14, ("Overflow", "Overflow"))
  , (15, ("InvalidLength", "Invalid length"))
  , (16, ("ProtocolError", "Protocol error"))
  , (17, ("AuthenticationFailed", "Authentication failed"))
  , (18, ("CannotReuseObject", "Cannot reuse object"))
  , (20, ("IllegalOperation", "Illegal operation"))
  , (21, ("EmptyArrayOperation", "Empty array operation"))
  , (22, ("InvalidBSON", "Invalid BSON"))
  , (23, ("AlreadyInitialized", "Already initialized"))
  , (24, ("LockTimeout", "Lock timeout"))
  , (25, ("RemoteValidationError", "Remote validation error"))
  , (26, ("NamespaceNotFound", "Namespace not found"))
  , (27, ("IndexNotFound", "Index not found"))
  , (28, ("PathNotViable", "Path not viable"))
  , (29, ("NonExistentPath", "Path does not exist"))
  , (30, ("InvalidPath", "Invalid path"))
  , (31, ("RoleNotFound", "Role not found"))
  , (32, ("RolesNotRelated", "Roles not related"))
  , (33, ("PrivilegeNotFound", "Privilege not found"))
  , (34, ("CannotBackfillArray", "Cannot backfill array"))
  , (35, ("UserModificationFailed", "User modification failed"))
  , (36, ("RemoteChangeDetected", "Remote change detected"))
  , (37, ("FileRenameFailed", "File rename failed"))
  , (38, ("FileNotOpen", "File not open"))
  , (39, ("FileStreamFailed", "File stream failed"))
  , (40, ("ConflictingUpdateOperators", "Conflicting update operators"))
  , (41, ("FileAlreadyOpen", "File already open"))
  , (46, ("LockBusy", "Lock busy"))
  , (47, ("CursorNotFound", "Cursor not found"))
  , (48, ("PrepareConflict", "Prepare conflict"))
  , (50, ("ExceededTimeLimit", "Exceeded time limit"))
  , (51, ("ExceededMemoryLimit", "Exceeded memory limit"))
  , (61, ("ShardKeyNotFound", "Shard key not found"))
  , (62, ("OplogOperationUnsupported", "Oplog operation unsupported"))
  , (63, ("StaleShardVersion", "Stale shard version"))
  , (67, ("CannotCreateCollection", "Cannot create collection"))
  , (68, ("CannotDropCollection", "Cannot drop collection"))
  , (72, ("InvalidOptions", "Invalid options"))
  , (73, ("InvalidNamespace", "Invalid namespace"))
  , (74, ("NodeNotFound", "Node not found"))
  , (85, ("IndexKeySpecsConflict", "Index key specs conflict"))
  , (86, ("CannotSplit", "Cannot split"))
  , (89, ("NetworkTimeout", "Network timeout"))
  , (91, ("ShutdownInProgress", "Shutdown in progress"))
  , (96, ("OperationFailed", "Operation failed"))
  , (112, ("WriteConflict", "Write conflict"))
  , (115, ("CommandNotSupported", "Command not supported"))
  , (117, ("ConflictingOperationInProgress", "Conflicting operation in progress"))
  , (133, ("FailedToSatisfyReadPreference", "Failed to satisfy read preference"))
  , (211, ("KeyNotFound", "Key not found"))
  , (11000, ("DuplicateKey", "Duplicate key error"))
  , (11001, ("DuplicateKeyIndex", "Duplicate key in unique index"))
  , (12586, ("BackgroundOperationInProgress", "Background operation in progress"))
  , (13297, ("DatabaseDifferCase", "Database differs by case"))
  , (17280, ("KeyTooLong", "Key too long"))
  ]

--------------------------------------------------------------------------------
-- Elasticsearch Errors
--------------------------------------------------------------------------------
elasticCodes :: Map Int (String, String)
elasticCodes = Map.fromList
  [ (400, ("bad_request", "Invalid request"))
  , (401, ("unauthorized", "Authentication required"))
  , (403, ("forbidden", "Permission denied"))
  , (404, ("index_not_found_exception", "Index not found"))
  , (405, ("method_not_allowed", "Method not allowed"))
  , (408, ("request_timeout", "Request timeout"))
  , (409, ("version_conflict_engine_exception", "Version conflict"))
  , (413, ("request_too_large", "Request too large"))
  , (429, ("too_many_requests", "Too many requests"))
  , (500, ("internal_server_error", "Internal server error"))
  , (503, ("unavailable", "Service unavailable"))
  , (504, ("gateway_timeout", "Gateway timeout"))
  ]

--------------------------------------------------------------------------------
-- Kubernetes Errors
--------------------------------------------------------------------------------
k8sCodes :: Map String (String, String, Int)
k8sCodes = Map.fromList
  [ ("BadRequest", ("BadRequest", "Invalid request", 400))
  , ("Unauthorized", ("Unauthorized", "Not authorized", 401))
  , ("Forbidden", ("Forbidden", "Access denied", 403))
  , ("NotFound", ("NotFound", "Resource not found", 404))
  , ("MethodNotAllowed", ("MethodNotAllowed", "Method not allowed", 405))
  , ("NotAcceptable", ("NotAcceptable", "Not acceptable", 406))
  , ("AlreadyExists", ("AlreadyExists", "Resource already exists", 409))
  , ("Conflict", ("Conflict", "Resource conflict", 409))
  , ("Gone", ("Gone", "Resource no longer available", 410))
  , ("Invalid", ("Invalid", "Invalid resource", 422))
  , ("TooManyRequests", ("TooManyRequests", "Rate limited", 429))
  , ("InternalError", ("InternalError", "Internal server error", 500))
  , ("ServiceUnavailable", ("ServiceUnavailable", "Service unavailable", 503))
  , ("Timeout", ("Timeout", "Request timeout", 504))
  , ("ServerTimeout", ("ServerTimeout", "Server timeout", 504))
  -- Extended
  , ("Expired", ("Expired", "Resource expired", 410))
  , ("Outdated", ("Outdated", "Resource is outdated", 409))
  ]

--------------------------------------------------------------------------------
-- Docker Errors
--------------------------------------------------------------------------------
dockerCodes :: Map String (String, String, Int)
dockerCodes = Map.fromList
  [ ("ContainerNotFound", ("container_not_found", "Container not found", 404))
  , ("ImageNotFound", ("image_not_found", "Image not found", 404))
  , ("NetworkNotFound", ("network_not_found", "Network not found", 404))
  , ("VolumeNotFound", ("volume_not_found", "Volume not found", 404))
  , ("ContainerAlreadyExists", ("container_exists", "Container already exists", 409))
  , ("ContainerNotRunning", ("container_not_running", "Container not running", 409))
  , ("ContainerRunning", ("container_running", "Container is running", 409))
  , ("PortInUse", ("port_in_use", "Port already in use", 500))
  , ("OutOfMemory", ("out_of_memory", "Out of memory", 500))
  , ("DiskFull", ("disk_full", "Disk is full", 500))
  , ("PermissionDenied", ("permission_denied", "Permission denied", 403))
  , ("Unauthorized", ("unauthorized", "Authentication required", 401))
  , ("BadRequest", ("bad_request", "Invalid request", 400))
  , ("ServerError", ("server_error", "Server error", 500))
  ]

--------------------------------------------------------------------------------
-- Firebase Errors
--------------------------------------------------------------------------------
firebaseCodes :: Map String (String, String, Int)
firebaseCodes = Map.fromList
  [ ("INVALID_ARGUMENT", ("INVALID_ARGUMENT", "Invalid argument", 400))
  , ("FAILED_PRECONDITION", ("FAILED_PRECONDITION", "Failed precondition", 400))
  , ("OUT_OF_RANGE", ("OUT_OF_RANGE", "Value out of range", 400))
  , ("UNAUTHENTICATED", ("UNAUTHENTICATED", "Not authenticated", 401))
  , ("PERMISSION_DENIED", ("PERMISSION_DENIED", "Permission denied", 403))
  , ("NOT_FOUND", ("NOT_FOUND", "Resource not found", 404))
  , ("ABORTED", ("ABORTED", "Operation aborted", 409))
  , ("ALREADY_EXISTS", ("ALREADY_EXISTS", "Resource already exists", 409))
  , ("RESOURCE_EXHAUSTED", ("RESOURCE_EXHAUSTED", "Resource exhausted", 429))
  , ("CANCELLED", ("CANCELLED", "Operation cancelled", 499))
  , ("DATA_LOSS", ("DATA_LOSS", "Unrecoverable data loss", 500))
  , ("UNKNOWN", ("UNKNOWN", "Unknown error", 500))
  , ("INTERNAL", ("INTERNAL", "Internal error", 500))
  , ("UNIMPLEMENTED", ("UNIMPLEMENTED", "Not implemented", 501))
  , ("UNAVAILABLE", ("UNAVAILABLE", "Service unavailable", 503))
  , ("DEADLINE_EXCEEDED", ("DEADLINE_EXCEEDED", "Deadline exceeded", 504))
  ]

--------------------------------------------------------------------------------
-- Twilio Errors
--------------------------------------------------------------------------------
twilioCodes :: Map Int (String, String, Int)
twilioCodes = Map.fromList
  [ (20001, ("Invalid request", "Request validation failed", 400))
  , (20003, ("Authentication failed", "Invalid credentials", 401))
  , (20004, ("Method not allowed", "Method not allowed", 405))
  , (20005, ("Account not active", "Account suspended", 403))
  , (20006, ("Access denied", "Access denied to resource", 403))
  , (20008, ("Resource not found", "Resource does not exist", 404))
  , (20404, ("Not found", "Resource not found", 404))
  , (20429, ("Too many requests", "Rate limit exceeded", 429))
  , (21201, ("Invalid phone number", "Invalid phone number format", 400))
  , (21210, ("Phone number not verified", "Number not verified", 400))
  , (21211, ("Invalid To number", "Invalid destination number", 400))
  , (21212, ("Invalid From number", "Invalid source number", 400))
  , (21401, ("Invalid phone number", "Phone number is invalid", 400))
  , (21408, ("Permission denied", "Geographic permission needed", 403))
  , (21610, ("Unsubscribed recipient", "Recipient unsubscribed", 400))
  , (21611, ("Invalid From number", "From number not SMS capable", 400))
  , (21612, ("To number not SMS capable", "Cannot send SMS to landline", 400))
  , (21614, ("Invalid mobile number", "Not a valid mobile number", 400))
  , (21617, ("Message body required", "Message body is required", 400))
  , (30001, ("Queue overflow", "Message queue full", 500))
  , (30002, ("Account suspended", "Account suspended", 403))
  , (30003, ("Unreachable destination", "Destination unreachable", 400))
  , (30004, ("Message blocked", "Message blocked", 400))
  , (30005, ("Unknown destination", "Unknown destination handset", 400))
  , (30006, ("Landline unreachable", "Landline or unreachable carrier", 400))
  , (30007, ("Carrier violation", "Carrier content violation", 400))
  , (30008, ("Unknown error", "Unknown error", 500))
  ]

--------------------------------------------------------------------------------
-- SendGrid Errors
--------------------------------------------------------------------------------
sendgridCodes :: Map Int (String, String)
sendgridCodes = Map.fromList
  [ (400, ("Bad request", "Invalid request parameters"))
  , (401, ("Unauthorized", "Invalid API key"))
  , (403, ("Forbidden", "Access denied"))
  , (404, ("Not found", "Resource not found"))
  , (405, ("Method not allowed", "Method not allowed"))
  , (413, ("Payload too large", "Request body too large"))
  , (415, ("Unsupported media type", "Unsupported content type"))
  , (429, ("Too many requests", "Rate limit exceeded"))
  , (500, ("Server error", "Internal server error"))
  , (503, ("Service unavailable", "Service temporarily unavailable"))
  ]

--------------------------------------------------------------------------------
-- Cloudflare Errors
--------------------------------------------------------------------------------
cloudflareCodes :: Map Int (String, String)
cloudflareCodes = Map.fromList
  [ (520, ("Unknown Error", "Origin returned unexpected response"))
  , (521, ("Web Server Is Down", "Origin server refused connection"))
  , (522, ("Connection Timed Out", "Connection to origin timed out"))
  , (523, ("Origin Is Unreachable", "Could not reach origin server"))
  , (524, ("A Timeout Occurred", "Connection established but response timed out"))
  , (525, ("SSL Handshake Failed", "SSL handshake with origin failed"))
  , (526, ("Invalid SSL Certificate", "Origin SSL certificate invalid"))
  , (527, ("Railgun Error", "Interrupted connection to Railgun"))
  , (530, ("Origin DNS Error", "DNS resolution for origin failed"))
  ]

--------------------------------------------------------------------------------
-- Vercel Errors
--------------------------------------------------------------------------------
vercelCodes :: Map String (String, String, Int)
vercelCodes = Map.fromList
  [ ("BAD_REQUEST", ("BAD_REQUEST", "Invalid request", 400))
  , ("FORBIDDEN", ("FORBIDDEN", "Access denied", 403))
  , ("NOT_FOUND", ("NOT_FOUND", "Resource not found", 404))
  , ("METHOD_NOT_ALLOWED", ("METHOD_NOT_ALLOWED", "Method not allowed", 405))
  , ("CONFLICT", ("CONFLICT", "Resource conflict", 409))
  , ("RATE_LIMITED", ("RATE_LIMITED", "Rate limit exceeded", 429))
  , ("INTERNAL_SERVER_ERROR", ("INTERNAL_SERVER_ERROR", "Server error", 500))
  , ("DEPLOYMENT_NOT_FOUND", ("DEPLOYMENT_NOT_FOUND", "Deployment not found", 404))
  , ("DOMAIN_NOT_FOUND", ("DOMAIN_NOT_FOUND", "Domain not found", 404))
  , ("PROJECT_NOT_FOUND", ("PROJECT_NOT_FOUND", "Project not found", 404))
  , ("TEAM_NOT_FOUND", ("TEAM_NOT_FOUND", "Team not found", 404))
  , ("BUILD_FAILED", ("BUILD_FAILED", "Build failed", 500))
  , ("FUNCTION_INVOCATION_FAILED", ("FUNCTION_INVOCATION_FAILED", "Function invocation failed", 500))
  , ("FUNCTION_INVOCATION_TIMEOUT", ("FUNCTION_INVOCATION_TIMEOUT", "Function timed out", 504))
  ]

--------------------------------------------------------------------------------
-- Supabase Errors
--------------------------------------------------------------------------------
supabaseCodes :: Map String (String, String, Int)
supabaseCodes = Map.fromList
  [ ("invalid_request", ("invalid_request", "Invalid request", 400))
  , ("unauthorized", ("unauthorized", "Not authorized", 401))
  , ("invalid_credentials", ("invalid_credentials", "Invalid credentials", 401))
  , ("email_not_confirmed", ("email_not_confirmed", "Email not confirmed", 401))
  , ("phone_not_confirmed", ("phone_not_confirmed", "Phone not confirmed", 401))
  , ("bad_jwt", ("bad_jwt", "Invalid JWT", 401))
  , ("expired_token", ("expired_token", "Token expired", 401))
  , ("forbidden", ("forbidden", "Access forbidden", 403))
  , ("not_found", ("not_found", "Resource not found", 404))
  , ("conflict", ("conflict", "Resource conflict", 409))
  , ("user_already_exists", ("user_already_exists", "User already exists", 409))
  , ("email_exists", ("email_exists", "Email already registered", 409))
  , ("phone_exists", ("phone_exists", "Phone already registered", 409))
  , ("rate_limit", ("rate_limit", "Rate limit exceeded", 429))
  , ("over_request_rate_limit", ("over_request_rate_limit", "Too many requests", 429))
  , ("over_email_send_rate_limit", ("over_email_send_rate_limit", "Email rate limit exceeded", 429))
  , ("over_sms_send_rate_limit", ("over_sms_send_rate_limit", "SMS rate limit exceeded", 429))
  , ("internal_error", ("internal_error", "Internal error", 500))
  ]

--------------------------------------------------------------------------------
-- HTCPCP (Hyper Text Coffee Pot Control Protocol) - RFC 2324 & RFC 7168
--------------------------------------------------------------------------------
htcpcpCodes :: Map Int (String, String)
htcpcpCodes = Map.fromList
  [ (418, ("I'm a teapot", "The server is a teapot and cannot brew coffee (RFC 2324)"))
  , (300, ("Multiple Choices", "Multiple coffee types available"))
  , (400, ("Bad Request", "Malformed coffee request"))
  , (403, ("Forbidden", "Coffee brewing is forbidden"))
  , (404, ("Not Found", "No coffee pot found at this URI"))
  , (406, ("Not Acceptable", "The requested addition is not available"))
  , (408, ("Request Timeout", "Coffee brewing timed out"))
  , (410, ("Gone", "The coffee pot has been permanently removed"))
  , (418, ("I'm a teapot", "The server refuses to brew coffee because it is a teapot"))
  , (500, ("Internal Server Error", "Coffee pot malfunction"))
  , (503, ("Service Unavailable", "Coffee pot is temporarily out of service"))
  ]

htcpcpStrCodes :: Map String (String, String, Int)
htcpcpStrCodes = Map.fromList
  -- RFC 2324 additions
  [ ("teapot", ("I'm a teapot", "This server is a teapot, not a coffee pot", 418))
  , ("no-coffee", ("I'm a teapot", "The requested entity body is short and stout", 418))
  -- Milk-related errors (RFC 7168)
  , ("cream", ("Not Acceptable", "Cream is not available", 406))
  , ("half-and-half", ("Not Acceptable", "Half-and-half is not available", 406))
  , ("whole-milk", ("Not Acceptable", "Whole milk is not available", 406))
  , ("skim-milk", ("Not Acceptable", "Skim milk is not available", 406))
  , ("oat-milk", ("Not Acceptable", "Oat milk is not available", 406))
  , ("almond-milk", ("Not Acceptable", "Almond milk is not available", 406))
  , ("soy-milk", ("Not Acceptable", "Soy milk is not available", 406))
  -- Sweetener errors
  , ("sugar", ("Not Acceptable", "Sugar is not available", 406))
  , ("sweetener", ("Not Acceptable", "Artificial sweetener is not available", 406))
  , ("honey", ("Not Acceptable", "Honey is not available", 406))
  -- Brewing errors
  , ("empty", ("Service Unavailable", "Coffee pot is empty", 503))
  , ("brewing", ("Service Unavailable", "Coffee is currently brewing, please wait", 503))
  , ("overflow", ("Internal Server Error", "Coffee pot overflow detected", 500))
  , ("grounds", ("Internal Server Error", "Coffee grounds container is full", 500))
  , ("water", ("Service Unavailable", "Water reservoir is empty", 503))
  , ("filter", ("Service Unavailable", "Coffee filter needs replacement", 503))
  , ("descale", ("Service Unavailable", "Coffee pot needs descaling", 503))
  , ("hot", ("Service Unavailable", "Coffee pot is too hot, cooling down", 503))
  , ("cold", ("Service Unavailable", "Coffee pot is heating up, please wait", 503))
  -- Pot status
  , ("off", ("Service Unavailable", "Coffee pot is turned off", 503))
  , ("unplugged", ("Service Unavailable", "Coffee pot is unplugged", 503))
  , ("missing", ("Not Found", "Coffee pot has been removed", 404))
  , ("stolen", ("Gone", "Coffee pot has been permanently removed (stolen)", 410))
  -- Authentication
  , ("unauthorized", ("Unauthorized", "Coffee brewing requires authentication", 401))
  , ("forbidden", ("Forbidden", "You are not allowed to brew coffee", 403))
  , ("quota", ("Too Many Requests", "Daily coffee quota exceeded", 429))
  ]

--------------------------------------------------------------------------------
-- HTML Pages
--------------------------------------------------------------------------------
homePage :: ByteString
homePage = BS.pack $ unlines
  [ "<!DOCTYPE html><html><head>"
  , "<meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
  , "<title>errors.garden</title>"
  , "<style>"
  , "body{font-family:monospace;font-size:12px;line-height:1.5;background:#f7f7f8;color:#111;margin:0;display:flex;height:100vh}"
  , "a{color:#111}"
  , ".left{padding:40px;width:50%;box-sizing:border-box}"
  , ".right{width:50%;border-left:1px solid #ddd;overflow-y:auto;padding:40px;box-sizing:border-box}"
  , ".right::-webkit-scrollbar{width:4px}"
  , ".right::-webkit-scrollbar-track{background:transparent}"
  , ".right::-webkit-scrollbar-thumb{background:#ccc;border-radius:2px}"
  , ".right::-webkit-scrollbar-thumb:hover{background:#999}"
  , "h2{margin-top:24px;margin-bottom:8px;padding-top:16px}"
  , "h2:first-child{margin-top:0;padding-top:0}"
  , "td{padding:2px 12px 2px 0;vertical-align:top}"
  , ".service{cursor:pointer}"
  , ".service:hover{text-decoration:underline}"
  , "@media(max-width:800px){body{flex-direction:column;height:auto}.left,.right{width:100%}.right{border-left:none;border-top:1px solid #ddd;max-height:50vh}}"
  , "</style>"
  , "</head><body>"
  , ""
  , "<div class=\"left\">"
  , "ERRORS.GARDEN"
  , "<br><br>"
  , "a garden of errors"
  , "<br><br>"
  , "~~~"
  , "<br><br>"
  , "we build tools for testing error handling.<br>"
  , "hit any endpoint to receive that error."
  , "<br><br>"
  , "~~~"
  , "<br><br>"
  , "services"
  , "<br><br>"
  , "- <a class=\"service\" data-target=\"http\">/http/{code}</a> - returns actual http status<br>"
  , "- <a class=\"service\" data-target=\"ws\">/ws/{code}</a> - websocket close codes<br>"
  , "- <a class=\"service\" data-target=\"grpc\">/grpc/{code}</a> - grpc status codes<br>"
  , "- <a class=\"service\" data-target=\"mcp\">/mcp/{code}</a> - mcp/json-rpc errors<br>"
  , "- <a class=\"service\" data-target=\"gql\">/gql/{type}</a> - graphql errors<br>"
  , "- <a class=\"service\" data-target=\"fastapi\">/fastapi/{code}</a> - fastapi errors<br>"
  , "- <a class=\"service\" data-target=\"pydantic\">/pydantic/{type}</a> - pydantic validation<br>"
  , "- <a class=\"service\" data-target=\"oauth\">/oauth/{type}</a> - oauth 2.0 errors<br>"
  , "- <a class=\"service\" data-target=\"aws\">/aws/{type}</a> - aws errors<br>"
  , "- <a class=\"service\" data-target=\"stripe\">/stripe/{type}</a> - stripe errors<br>"
  , "- <a class=\"service\" data-target=\"postgres\">/postgres/{code}</a> - postgresql<br>"
  , "- <a class=\"service\" data-target=\"mysql\">/mysql/{code}</a> - mysql errors<br>"
  , "- <a class=\"service\" data-target=\"redis\">/redis/{type}</a> - redis errors<br>"
  , "- <a class=\"service\" data-target=\"mongo\">/mongo/{code}</a> - mongodb errors<br>"
  , "- <a class=\"service\" data-target=\"elastic\">/elastic/{code}</a> - elasticsearch<br>"
  , "- <a class=\"service\" data-target=\"k8s\">/k8s/{reason}</a> - kubernetes<br>"
  , "- <a class=\"service\" data-target=\"docker\">/docker/{type}</a> - docker<br>"
  , "- <a class=\"service\" data-target=\"firebase\">/firebase/{code}</a> - firebase<br>"
  , "- <a class=\"service\" data-target=\"twilio\">/twilio/{code}</a> - twilio<br>"
  , "- <a class=\"service\" data-target=\"sendgrid\">/sendgrid/{code}</a> - sendgrid<br>"
  , "- <a class=\"service\" data-target=\"cloudflare\">/cloudflare/{code}</a> - cloudflare<br>"
  , "- <a class=\"service\" data-target=\"vercel\">/vercel/{code}</a> - vercel<br>"
  , "- <a class=\"service\" data-target=\"supabase\">/supabase/{code}</a> - supabase<br>"
  , "- <a class=\"service\" data-target=\"htcpcp\">/htcpcp/{code}</a> - coffee pot protocol<br>"
  , "<br>"
  , "~~~"
  , "<br><br>"
  , "<a href=\"mailto:hello@errors.garden\">hello@errors.garden</a>"
  , "</div>"
  , ""
  , "<div class=\"right\" id=\"ref\">"
  , refContent
  , "</div>"
  , ""
  , "<script>"
  , "document.querySelectorAll('.service').forEach(a=>{"
  , "a.onclick=e=>{e.preventDefault();const t=document.getElementById(a.dataset.target);if(t){t.scrollIntoView({behavior:'smooth',block:'start'});}};"
  , "});"
  , "</script>"
  , "</body></html>"
  ]

refContent :: String
refContent = unlines
  [ refSect "http" "HTTP" "/http/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList httpCodes
  , refSect "ws" "WebSocket" "/ws/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList wsCodes
  , refSect "grpc" "gRPC" "/grpc/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList grpcCodes
  , refSect "mcp" "MCP" "/mcp/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList mcpCodes
  , refSect "gql" "GraphQL" "/gql/{type}" $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList gqlCodes
  , refSect "fastapi" "FastAPI" "/fastapi/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList fastapiCodes
  , refSect "pydantic" "Pydantic" "/pydantic/{type}" $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList pydanticCodes
  , refSect "oauth" "OAuth" "/oauth/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList oauthCodes
  , refSect "aws" "AWS" "/aws/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList awsCodes
  , refSect "stripe" "Stripe" "/stripe/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList stripeCodes
  , refSect "postgres" "PostgreSQL" "/postgres/{code}" $ take 40 $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList pgCodes
  , refSect "mysql" "MySQL" "/mysql/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList mysqlCodes
  , refSect "redis" "Redis" "/redis/{type}" $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList redisCodes
  , refSect "mongo" "MongoDB" "/mongo/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList mongoCodes
  , refSect "elastic" "Elasticsearch" "/elastic/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList elasticCodes
  , refSect "k8s" "Kubernetes" "/k8s/{reason}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList k8sCodes
  , refSect "docker" "Docker" "/docker/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList dockerCodes
  , refSect "firebase" "Firebase" "/firebase/{code}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList firebaseCodes
  , refSect "twilio" "Twilio" "/twilio/{code}" $ map (\(c,(n,_,_)) -> (show c, n)) $ Map.toAscList twilioCodes
  , refSect "sendgrid" "SendGrid" "/sendgrid/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList sendgridCodes
  , refSect "cloudflare" "Cloudflare" "/cloudflare/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList cloudflareCodes
  , refSect "vercel" "Vercel" "/vercel/{code}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList vercelCodes
  , refSect "supabase" "Supabase" "/supabase/{code}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList supabaseCodes
  , refSect "htcpcp" "HTCPCP" "/htcpcp/{code}" $ map (\(c,(n,_)) -> (show c, n)) (Map.toAscList htcpcpCodes) ++ map (\(c,(n,_,_)) -> (c, n)) (Map.toAscList htcpcpStrCodes)
  ]

refSect :: String -> String -> String -> [(String, String)] -> String
refSect anchor name endpoint codes = unlines $
  [ "<h2 id=\"" ++ anchor ++ "\">" ++ name ++ " (" ++ endpoint ++ ")</h2>"
  , "<table>"
  ] ++ map row codes ++ ["</table>"]
  where row (c, n) = "<tr><td>" ++ c ++ "</td><td>" ++ n ++ "</td></tr>"

errorsPage :: ByteString
errorsPage = BS.pack $ unlines
  [ "<!DOCTYPE html><html><head>"
  , "<meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
  , "<title>errors | errors.garden</title>"
  , "<style>body{font-family:monospace;font-size:12px;line-height:1.5;background:#f7f7f8;color:#111;padding:40px;margin:0}a{color:#111}h2{margin-top:32px;margin-bottom:8px}td{padding:2px 16px 2px 0;vertical-align:top}</style>"
  , "</head><body>"
  , ""
  , "<a href=\"/\">ERRORS.GARDEN</a> / errors"
  , ""
  , "<br><br>"
  , ""
  , "~~~"
  , ""
  , "<br>"
  , ""
  , sectionHttp
  , sectionWs
  , sectionGrpc
  , sectionMcp
  , sectionGql
  , sectionFastapi
  , sectionPydantic
  , sectionOauth
  , sectionAws
  , sectionStripe
  , sectionPg
  , sectionMysql
  , sectionRedis
  , sectionMongo
  , sectionElastic
  , sectionK8s
  , sectionDocker
  , sectionFirebase
  , sectionTwilio
  , sectionSendgrid
  , sectionCloudflare
  , sectionVercel
  , sectionSupabase
  , sectionHtcpcp
  , ""
  , "<br>"
  , "~~~"
  , "<br><br>"
  , "<a href=\"/\">[back]</a>"
  , ""
  , "</body></html>"
  ]

sectionHttp, sectionWs, sectionGrpc, sectionMcp, sectionGql :: String
sectionFastapi, sectionPydantic, sectionOauth, sectionAws, sectionStripe :: String
sectionPg, sectionMysql, sectionRedis, sectionMongo, sectionElastic :: String
sectionK8s, sectionDocker, sectionFirebase, sectionTwilio, sectionSendgrid :: String
sectionCloudflare, sectionVercel, sectionSupabase, sectionHtcpcp :: String

sectionHttp = sect "HTTP" "/http/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList httpCodes
sectionWs = sect "WebSocket" "/ws/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList wsCodes
sectionGrpc = sect "gRPC" "/grpc/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList grpcCodes
sectionMcp = sect "MCP/JSON-RPC" "/mcp/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList mcpCodes
sectionGql = sect "GraphQL" "/gql/{type}" $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList gqlCodes
sectionFastapi = sect "FastAPI" "/fastapi/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList fastapiCodes
sectionPydantic = sect "Pydantic" "/pydantic/{type}" $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList pydanticCodes
sectionOauth = sect "OAuth 2.0" "/oauth/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList oauthCodes
sectionAws = sect "AWS" "/aws/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList awsCodes
sectionStripe = sect "Stripe" "/stripe/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList stripeCodes
sectionPg = sect "PostgreSQL" "/postgres/{code}" $ take 40 $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList pgCodes
sectionMysql = sect "MySQL" "/mysql/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList mysqlCodes
sectionRedis = sect "Redis" "/redis/{type}" $ map (\(c,(n,_)) -> (c, n)) $ Map.toAscList redisCodes
sectionMongo = sect "MongoDB" "/mongo/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList mongoCodes
sectionElastic = sect "Elasticsearch" "/elastic/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList elasticCodes
sectionK8s = sect "Kubernetes" "/k8s/{reason}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList k8sCodes
sectionDocker = sect "Docker" "/docker/{type}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList dockerCodes
sectionFirebase = sect "Firebase" "/firebase/{code}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList firebaseCodes
sectionTwilio = sect "Twilio" "/twilio/{code}" $ map (\(c,(n,_,_)) -> (show c, n)) $ Map.toAscList twilioCodes
sectionSendgrid = sect "SendGrid" "/sendgrid/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList sendgridCodes
sectionCloudflare = sect "Cloudflare" "/cloudflare/{code}" $ map (\(c,(n,_)) -> (show c, n)) $ Map.toAscList cloudflareCodes
sectionVercel = sect "Vercel" "/vercel/{code}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList vercelCodes
sectionSupabase = sect "Supabase" "/supabase/{code}" $ map (\(c,(n,_,_)) -> (c, n)) $ Map.toAscList supabaseCodes
sectionHtcpcp = sect "HTCPCP" "/htcpcp/{code}" $ map (\(c,(n,_)) -> (show c, n)) (Map.toAscList htcpcpCodes) ++ map (\(c,(n,_,_)) -> (c, n)) (Map.toAscList htcpcpStrCodes)

sect :: String -> String -> [(String, String)] -> String
sect name endpoint codes = unlines $
  [ "<h2>" ++ name ++ " (" ++ endpoint ++ ")</h2>"
  , "<table>"
  ] ++ map row codes ++ ["</table>"]
  where row (c, n) = "<tr><td>" ++ c ++ "</td><td>" ++ n ++ "</td></tr>"
