{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module App where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.StripHeaders (stripHeader)
import           Servant
import           System.IO
import           Crypto.BCrypt
import qualified Data.Map as Map
import           Data.Time
import           Data.UUID
import           Data.UUID.V4
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.ByteString.Lazy as BL
import           Control.Exception (try)
import           Control.Monad (when)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, stateTVar)
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Data.List (partition, null)
import           Data.IORef
import           Text.Toml
import           Options.Applicative
import           Data.Text (pack)
import           Text.ParserCombinators.Parsec.Error
import           Data.Either.Combinators
import           System.Exit

-- * Application state, will be lifted into a TVar later on

-- What kind of request do we have?
data RequestType = Question | Submission
  deriving (Generic, Show, Eq)
instance ToJSON RequestType
instance FromJSON RequestType

type NumericAddress = String

-- What User are we talking about
data Identification = RemoteIP NumericAddress | AuthUser String | Anonymous
  deriving (Generic, Show, Eq)
instance ToJSON Identification

data RequestID = RequestID UUID | BlindedID
  deriving (Generic, Show, Eq)
instance ToJSON RequestID
instance FromJSON RequestID

data HelpRequest = HelpRequest { reqid :: RequestID, userid :: Identification, time :: UTCTime, reqtype :: RequestType }
  deriving (Generic, Show, Eq)
instance ToJSON HelpRequest

newtype Responder = Responder String
  deriving (Generic, Show, Eq, Ord)
  deriving ToJSONKey via String
instance ToJSON Responder

data State = State { lectureName     :: String
                   , timeSlots       :: [TimeRange]
                   , conferenceUrl   :: String
                   , activeRequests  :: Map.Map Responder HelpRequest
                   , pendingRequests :: [ HelpRequest ]
                   , actionLog       :: [ LogItem ]
                   , backlogMinutes  :: Integer
                   }
  deriving (Generic, Show, Eq)
instance ToJSON State

data LogVerb = UserLogAction UserVerb | AdminLogAction AdminVerb
  deriving (Generic, Show, Eq)
instance ToJSON LogVerb
data LogItem = LogItem { action    :: LogVerb
                       , timeStamp :: UTCTime
                       , actor     :: String
                       , requests  :: [HelpRequest]
                       }
  deriving (Generic, Show, Eq)
instance ToJSON LogItem

data UserVerb = Create | Cancel
  deriving (Generic, Show, Eq)
instance ToJSON App.UserVerb
instance FromJSON App.UserVerb

data AdminVerb = Handle | Complete | Discard | Defer
  deriving (Generic, Show, Eq)
instance ToJSON App.AdminVerb
instance FromJSON App.AdminVerb

data AdminAction = AdminAction RequestID App.AdminVerb
  deriving (Generic, Show, Eq)
instance ToJSON AdminAction
instance FromJSON AdminAction

-- * Basic auth: https://docs.servant.dev/en/stable/cookbook/basic-auth/BasicAuth.html
type Username = String
type Password = String

data User = User
  { user  :: Username
  , pass  :: Password
  , admin :: Bool
  } deriving (Eq, Show, Generic)
instance FromJSON User
-- could be a postgres connection, a file, anything.
type UserDB = Map.Map Username User
data AuthConfig = AuthConfig
  { acceptanyusers :: Bool
  , authdb :: UserDB
  } deriving (Show, Generic)
instance FromJSON AuthConfig
-- create a "database" from a list of users
createUserDB :: [User] -> UserDB
createUserDB users = Map.fromList [ (user u, u) | u <- users ]

-- our test database
data Admin = Admin String | InvalidAdmin deriving (Eq, Show)

fromUser :: User -> Admin
fromUser User{user=u, admin=True} = Admin u
fromUser User{} = InvalidAdmin


checkBasicAuthCommon :: MonadIO m => UserDB -> Bool -> BasicAuthData -> m (BasicAuthResult User)
checkBasicAuthCommon db acceptanypassword basicAuthData = do
  let username = BS.toString $ BL.fromStrict (basicAuthUsername basicAuthData)
  let password = BS.toString $ BL.fromStrict (basicAuthPassword basicAuthData)
  case Map.lookup username db of
    Just u  -> if validatePw password (pass u)
               then return (Authorized u)
               else return BadPassword
    Nothing -> if acceptanypassword then
                 return $ Authorized $ User{user=username,admin=False,pass=""}
               else
                 return $ NoSuchUser
  where
    validatePw pw ref@('$':'2':'y':'$':_) = validatePassword (BL.toStrict $ BS.fromString ref)  (BL.toStrict $ BS.fromString pw)
    validatePw pw ref = ref == pw

-- provided we are given a user database, we can supply
-- a function that checks the basic auth credentials
-- against our database.
checkBasicAuth :: IORef AuthConfig -> BasicAuthCheck User
checkBasicAuth dbref = do
  BasicAuthCheck $ \basicAuthData -> do
    cfg <- liftIO $ readIORef dbref
    checkBasicAuthCommon (authdb (cfg :: AuthConfig)) (acceptanyusers (cfg :: AuthConfig)) basicAuthData

checkAdminBasicAuth :: IORef AuthConfig -> BasicAuthCheck Admin
checkAdminBasicAuth dbref = do
  BasicAuthCheck $ \basicAuthData -> do
    cfg <- liftIO $ readIORef dbref
    usercheck <- checkBasicAuthCommon (authdb (cfg :: AuthConfig)) False basicAuthData
    case usercheck of
        Authorized u -> do
            let useradmin = fromUser u
            case useradmin of
                InvalidAdmin -> return NoSuchUser
                _ -> return $ Authorized useradmin
        NoSuchUser -> return NoSuchUser
        BadPassword -> return BadPassword
        Unauthorized -> return Unauthorized

-- * Commandline Parsing

data CLIConfig = CLIConfig {
    port :: Port,
    configfile :: String
} deriving (Show)

commandline :: Options.Applicative.Parser CLIConfig
commandline = CLIConfig
        <$> option auto
            (long "port"
            <> short 'p'
            <> metavar "PORT"
            <> value 8080
            <> help "Port to listen to"
            )
        <*> strOption
            (long "configfile"
            <> short 'c'
            <> metavar "CONFIGURATIONFILE"
            <> value "./config.toml"
            <> help "Authentication database in TOML format"
            )

opts :: ParserInfo CLIConfig
opts = info (helper <*> commandline) (fullDesc <> header "Adora-Belle: Hands where I can see them")

-- * Config file

data TimeRange = TimeRange { day   :: DayOfWeek
                           , start :: TimeOfDay
                           , end   :: TimeOfDay
                           }
  deriving (Generic, Show, Eq)
instance ToJSON TimeRange
instance FromJSON TimeRange

data LectureConfig = LectureConfig {
    name                 :: String,
    conferenceurl        :: String,
    backlogminutes       :: Integer,
    timeslots            :: [TimeRange],
    acceptanyusers :: Bool,
    authdb               :: [User]
} deriving (Show, Generic, Eq)
instance FromJSON LectureConfig

-- Parse the authdb

parseLectureConfig :: String -> IO (Either String LectureConfig)
parseLectureConfig filepath = do
  filecontent <- try (readFile filepath)
  case filecontent of
    Left err -> return $ Left $ show (err :: IOError)
    Right txt -> do
        let parsed = parseTomlDoc (show filepath) $ pack txt
        return $ ((mapLeft (unlines . (map messageString) . errorMessages) parsed >>=
                    \x -> (parseEither $ parseJSON) $ toJSON x))

-- * api

type RequestApi =
  {- Request help of type RequestType, receives the new request -}
  "public" :> BasicAuth "AdoraBelle UI" User :>
        "request"  :> ReqBody '[JSON] RequestType :> PostCreated '[JSON] HelpRequest :<|>
  "public" :> BasicAuth "AdoraBelle UI" User :>
        "cancel"  :> ReqBody '[JSON] RequestID :> PostCreated '[JSON] [HelpRequest] :<|>
  {- Request an update of the global state -}
  "public" :> BasicAuth "AdoraBelle UI" User :>
        "requests" :> Get  '[JSON] State :<|>
  {- Request the deblinded state as admin -}
  "admin" :> BasicAuth "admin interface" Admin :>
        "requests" :> Get  '[JSON] State             :<|>
  {- Handle an request using Adminaction (so Handle it yourself, discard it, complete a currently running session, returns the modified helprequests -}
  "admin" :> BasicAuth "admin interface" Admin :>
        "handle"   :> ReqBody '[JSON] AdminAction :> PostCreated '[JSON] [HelpRequest] :<|>
  "admin" :> BasicAuth "admin interface" Admin :>
        "reload"   :> PostCreated '[JSON] State :<|>
  Raw

requestApi :: Proxy RequestApi
requestApi = Proxy

-- * Lift STM into a Handler: https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html

data ServantState = ServantState {state :: TVar State, configfile :: FilePath, userdb :: IORef AuthConfig}
type AppM = ReaderT ServantState Handler

nt :: ServantState -> AppM a -> Handler a
nt s x = runReaderT x s

-- * app

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

run :: IO ()
run = do
  -- CMDLINE Parsing
  config <- execParser opts
  let filepath = (configfile (config :: CLIConfig))
  fileconf <- parseLectureConfig filepath
  when (isLeft fileconf) $
    exitWithErrorMessage ("Failed to parse config file: " ++ filepath ++ "\n" ++ (fromLeft' fileconf)) (ExitFailure 2)

  let configdata = (fromRight' fileconf) :: LectureConfig

  let lport = port config
      settings =
        setPort lport $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show lport)) $
        defaultSettings

  appstate <- atomically $ newTVar $ State { lectureName = name $ configdata
                                           , conferenceUrl = conferenceurl $ configdata
                                           , timeSlots = timeslots $ configdata
                                           , activeRequests = Map.empty
                                           , pendingRequests = []
                                           , actionLog = []
                                           , backlogMinutes = backlogminutes $ configdata
                                           }
  dbref <- newIORef $ AuthConfig (acceptanyusers (configdata :: LectureConfig)) (createUserDB $ authdb (configdata :: LectureConfig))
  runSettings settings $ stripAuthenticateHeader $ mkApp dbref $ ServantState appstate (configfile (config :: CLIConfig)) dbref
  where
    stripAuthenticateHeader = (modifyResponse $ stripHeader "WWW-Authenticate") :: Middleware

mkApp :: IORef AuthConfig -> ServantState -> Application
mkApp dbref s = serveWithContext requestApi ctx $ hoistServerWithContext requestApi (Proxy :: Proxy '[BasicAuthCheck Admin, BasicAuthCheck User]) (nt s) server
  where ctx = checkAdminBasicAuth dbref :. checkBasicAuth dbref :. EmptyContext

-- Handlers
server :: ServerT RequestApi AppM
server =
  requestHelp  :<|>
  cancelRequest  :<|>
  requestPublicState :<|>
  requestAdminState :<|>
  handleAdminRequest :<|>
  handleReload :<|>
  serveDirectoryFileServer "./www"

expireLog :: Integer -> UTCTime -> [LogItem] -> [LogItem]
expireLog maxbacklog now log = filter (\LogItem{timeStamp=rtime} -> (diffUTCTime now rtime) < maxdiff) log
  where
    maxdiff = secondsToNominalDiffTime $ fromInteger $ 60 * maxbacklog

checkUserHasRequestsPending :: String -> State -> Bool
checkUserHasRequestsPending name State{activeRequests=a, pendingRequests=p} =
  any usermatches $ Prelude.map (\(HelpRequest _ ident _ _) -> ident) $ (Map.elems a) ++ p
  where
    usermatches (AuthUser uname) = name == uname
    usermatches _ = False

checkTimeslotsInTimeZone :: UTCTime -> TimeZone -> [TimeRange] -> Bool
checkTimeslotsInTimeZone _ _ [] = True
checkTimeslotsInTimeZone timestamp timezone timeslots =
  any (inslot efftime) timeslots
  where
    efftime = utcToLocalTime timezone timestamp
    inslot LocalTime{localDay=ld, localTimeOfDay=ldt} TimeRange{day=d,start=s,end=e} =
      (dayOfWeek ld == d) && (s <= ldt) && (ldt <= e)

requestHelp :: User -> RequestType -> AppM HelpRequest
requestHelp u rt = do
  ServantState{state=sest} <- ask
  uuid <- liftIO $ nextRandom
  ts   <- liftIO $ getCurrentTime
  tz   <- liftIO $ getCurrentTimeZone
  req  <- liftIO $ atomically $ stateTVar sest $ \s@State{activeRequests=a,pendingRequests=p,timeSlots=slots,actionLog=log,backlogMinutes=blm} ->
    if not $ checkTimeslotsInTimeZone ts tz  slots then
      (Left "Requests are only allowed during the designated timeslots", s)
    else if checkUserHasRequestsPending (user u) s then
        (Left "User already has a request pending", s)
    else do
        let req = HelpRequest (RequestID uuid) (AuthUser $ user u) ts rt
        let act = LogItem{action = UserLogAction Create, timeStamp = ts, actor = user u, requests = [req]}
        (Left "Sorry, the Tutors are busy, try again tomorrow", s)

  case req of
    Left errmsg  ->
      throwError $ err400 { errBody = errmsg }
    Right request ->
      return request

cancelRequest :: User -> RequestID -> AppM [HelpRequest]
cancelRequest u rid = do
  ts   <- liftIO $ getCurrentTime
  ServantState{state=sest} <- ask
  req  <- liftIO $ atomically $ stateTVar sest $ \s@State{activeRequests=a,pendingRequests=p,actionLog=log,backlogMinutes=blm} ->
    let removedactive = map snd $ findActive a
        remainingactive = foldr (\k m -> Map.delete k m) a $ map fst $ findActive a
        (matched, rest) = split p
        removed = matched ++ removedactive
    in
      if (Data.List.null removed) then
         (Left $ "No request matched uuid: " ++ (show rid), s)
      else if any (\req -> (userid req) /= (AuthUser $ user u)) removed then
         (Left $ "Access violation: You do not own request " ++ (show rid), s)
      else
         let act = LogItem{action = UserLogAction Cancel, timeStamp = ts, actor = user u, requests = removed}
         in
           (Right removed, s{activeRequests=remainingactive, pendingRequests=rest, actionLog=expireLog blm ts (act:log)})

  case req of
    Left err  ->
      throwError $ err400 { errBody = BS.fromString err }
    Right reqs ->
      return reqs
  where
    matchesid req = rid == (reqid $ snd req)
    findActive = filter matchesid . Map.assocs
    split      = partition (\e -> reqid e == rid)

requestPublicState :: User -> AppM State
requestPublicState u = do
  ServantState{state=sest} <- ask
  s@State{activeRequests=a,pendingRequests=p} <- liftIO $ atomically $ readTVar sest
  let active = Map.map blind a
  let pend   = Prelude.map blind p
  return $ s{activeRequests=active, pendingRequests=pend, actionLog = []}
  where
    blind r@(HelpRequest _ (AuthUser au) _ _)
        | au == user u = r
        | otherwise = r { userid = Anonymous, reqid = BlindedID }
    blind r = r { userid = Anonymous, reqid = BlindedID }


requestAdminState  :: Admin -> AppM State
requestAdminState _ = do
  ServantState{state=sest} <- ask
  liftIO $ atomically $ readTVar sest

handleAdminRequest :: Admin -> AdminAction -> AppM [HelpRequest]
handleAdminRequest (Admin name) (AdminAction id act)   = do
  ts   <- liftIO $ getCurrentTime
  ServantState{state=sest} <- ask
  res  <- liftIO $ atomically $ stateTVar sest $ \s@State{activeRequests=a,pendingRequests=p,actionLog=log, backlogMinutes=blm} ->
    let expire      = expireLog blm ts
        logtemplate = LogItem{action = AdminLogAction act, timeStamp = ts, actor = name, requests = []}
    in
    case act of
      Complete ->
        let active = findActive a
            logitem =  logtemplate{requests = map snd $ active}
        in
          (Right $ map snd $ active, s{ activeRequests=(foldr (\k m -> Map.delete k m) a $ map fst $ active), actionLog=expire (logitem:log)})
      Handle   ->
        if Map.member (Responder name) a then
          (Left $ "Admin " ++ name ++ " is already handling an incident", s)
        else
          let (matched, rest) = split p
          in
            case matched of
                [] ->
                    (Left $ "No pending request matches RequestID " ++ (show id), s)
                [x] ->
                    let logitem = logtemplate{requests = [x]}
                    in
                      (Right matched, s{activeRequests=Map.insert (Responder name) x a, pendingRequests = rest, actionLog=expire (logitem:log)})
                _ ->
                    (Left $ "Internal Error: more than one Request with ID " ++ (show id) ++ " found, wtf?", s)
      Discard  ->
        let (matched, rest) = split p
            logitem = logtemplate{requests = matched}
        in
          (Right matched, s{pendingRequests = rest, actionLog=expire (logitem:log)})
      Defer    ->
        let active = findActive a
            reqs   = map snd $ active
            logitem = logtemplate{requests = reqs}
        in
          (Right $ reqs, s{ activeRequests=(foldr (\k m -> Map.delete k m) a $ map fst $ active), pendingRequests=reqs ++ p, actionLog=expire (logitem:log)})
  case res of
    Left err ->
      throwError $ err400 { errBody = BS.fromString err }
    Right reqs ->
      return reqs
  where
    matchesid req     = id == (reqid $ snd req)
    findActive        = filter matchesid . Map.assocs
    split             = partition (\e -> reqid e == id)
handleAdminRequest _ _  = throwError $ err400 { errBody = "Invalid user" }


handleReload :: Admin -> AppM State
handleReload (Admin _) = do
  ServantState{state=sest,configfile=fp,userdb=udbref} <- ask
  configdata <- liftIO $ parseLectureConfig fp
  case configdata of
    Left err -> do
      liftIO $ hPutStrLn stderr $ show err
      throwError $ err400 { errBody = "An error occoured while reloading configuration. Details were printed to servers STDERR (might contain sensitive data)" }
    Right LectureConfig{name=n,timeslots=ts,backlogminutes=backlog,authdb=adb,acceptanyusers=eup,conferenceurl=confurl} -> do
      liftIO $ writeIORef udbref $ AuthConfig eup (createUserDB adb)
      newstate <- liftIO $ atomically $ stateTVar sest $ \s ->
        let nstate = s{lectureName = n, timeSlots=ts, conferenceUrl=confurl, backlogMinutes=backlog} in (nstate, nstate)
      return newstate
handleReload _  = throwError $ err400 { errBody = "Invalid user" }
