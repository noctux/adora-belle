{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.JS
import           System.IO
import qualified Data.Map as Map
import           Data.Time
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4
import           System.Random
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.ByteString.Lazy as BL
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar, stateTVar)
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Exception (throwIO)
import           Data.List (partition)
import           Servant.Foreign
import qualified Data.Text as T
import           Servant.Auth hiding (BasicAuth)

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

data Responder = Responder String
  deriving (Generic, Show, Eq, Ord)
instance ToJSON Responder
instance ToJSONKey Responder

data State = State { activeRequests  :: Map.Map Responder HelpRequest
                   , pendingRequests :: [ HelpRequest ]
                   }
  deriving (Generic, Show, Eq)
instance ToJSON State
             
data Verb = Handle | Complete | Discard
  deriving (Generic, Show, Eq)
instance ToJSON App.Verb
instance FromJSON App.Verb

data AdminAction = AdminAction RequestID App.Verb
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
  } deriving (Eq, Show)
-- could be a postgres connection, a file, anything.
type UserDB = Map.Map Username User
-- create a "database" from a list of users
createUserDB :: [User] -> UserDB
createUserDB users = Map.fromList [ (user u, u) | u <- users ]

-- our test database
userDB :: UserDB
userDB = createUserDB
  [ User "user1" "pw" False
  , User "user2" "pw" False
  , User "user3" "pw" False
  , User "user4" "pw" False
  , User "user5" "pw" False
  , User "user6" "pw" False
  , User "user7" "pw" False
  , User "user8" "pw" False
  , User "admin1" "admin" True
  , User "admin2" "admin" True
  , User "admin3" "admin" True
  ]

data Admin = Admin String | InvalidAdmin deriving (Eq, Show)

fromUser :: User -> Admin
fromUser User{user=u, admin=True} = Admin u
fromUser User{} = InvalidAdmin

checkBasicAuthCommon :: Monad m => UserDB -> BasicAuthData -> m (BasicAuthResult User)
checkBasicAuthCommon db basicAuthData =
  let username = BS.toString $ BL.fromStrict (basicAuthUsername basicAuthData)
      password = BS.toString $ BL.fromStrict (basicAuthPassword basicAuthData)
  in
  case Map.lookup username db of
    Nothing -> return NoSuchUser
    Just u  -> if pass u == password
               then return (Authorized u)
               else return BadPassword
  
-- provided we are given a user database, we can supply
-- a function that checks the basic auth credentials
-- against our database.
checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db = BasicAuthCheck $ checkBasicAuthCommon db

                    
checkAdminBasicAuth :: UserDB -> BasicAuthCheck Admin
checkAdminBasicAuth db = BasicAuthCheck $ \basicAuthData -> do
  usercheck <- checkBasicAuthCommon db basicAuthData
  case usercheck of
    Authorized u -> do
      let useradmin = fromUser u
      case useradmin of
        InvalidAdmin -> return NoSuchUser
        _ -> return $ Authorized useradmin
    NoSuchUser -> return NoSuchUser
    BadPassword -> return BadPassword
    Unauthorized -> return Unauthorized
  
-- * api

type RequestApi =
  {- Reqeust help of type RequestType, receives the new request -}
  "public" :> BasicAuth "AdoraBelle UI" User :>
        "request"  :> ReqBody '[JSON] RequestType :> PostCreated '[JSON] HelpRequest :<|>
  {- Request an update of the global state -}
  "public" :> BasicAuth "AdoraBelle UI" User :>
        "requests" :> Get  '[JSON] State :<|>
  {- Request the deblinded state as admin -}
  "admin" :> BasicAuth "admin interface" Admin :>
        "requests" :> Get  '[JSON] State             :<|>
  {- Handle an request using Adminaction (so Handle it yourself, discard it, complete a currently running session, returns the modified helprequests -}
  "admin" :> BasicAuth "admin interface" Admin :>
        "handle"   :> ReqBody '[JSON] AdminAction :> PostCreated '[JSON] [HelpRequest] :<|>
  Raw

requestApi :: Proxy RequestApi
requestApi = Proxy

-- * Lift STM into a Handler: https://docs.servant.dev/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html

data ServantState = ServantState {state :: TVar State}
type AppM = ReaderT ServantState Handler

nt :: ServantState -> AppM a -> Handler a
nt s x = runReaderT x s

-- * app

run :: IO ()
run = do
  let port = 8080 
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings

  appstate <- atomically $ newTVar $ State {
                                           activeRequests = Map.empty,
                                           pendingRequests = []
                                           }
  runSettings settings $ mkApp $ ServantState appstate

mkApp :: ServantState -> Application
mkApp s = serveWithContext requestApi ctx $ hoistServerWithContext requestApi (Proxy :: Proxy '[BasicAuthCheck Admin, BasicAuthCheck User]) (nt s) server
  where ctx = checkAdminBasicAuth userDB :. checkBasicAuth userDB :. EmptyContext

-- Handlers
server :: ServerT RequestApi AppM
server = 
  requestHelp  :<|>
  requestPublicState :<|>
  requestAdminState :<|>
  handleAdminRequest :<|>
  serveDirectoryFileServer "./www"

checkUserHasRequestsPending :: String -> State -> Bool
checkUserHasRequestsPending name State{activeRequests=a, pendingRequests=p} =
  any usermatches $ Prelude.map (\(HelpRequest _ ident _ _) -> ident) $ (Map.elems a) ++ p
  where
    usermatches (AuthUser uname) = name == uname
    usermatches _ = False

requestHelp :: User -> RequestType -> AppM HelpRequest
requestHelp u rt = do
  ServantState{state=sest} <- ask
  uuid <- liftIO $ nextRandom
  ts   <- liftIO $ getCurrentTime
  req  <- liftIO $ atomically $ stateTVar sest $ \s@State{activeRequests=a,pendingRequests=p} ->
    if checkUserHasRequestsPending (user u) s then
        (Nothing, s)
    else do
        let req = HelpRequest (RequestID uuid) (AuthUser $ user u) ts rt
        (Just req, State a (p ++ [req]))

  case req of
    Nothing  ->
      liftIO $ throwIO $ err400 { errBody = "User already has a request pending" }
    Just req ->
      return req

requestPublicState :: User -> AppM State
requestPublicState u = do
  ServantState{state=sest} <- ask
  s@State{activeRequests=a,pendingRequests=p} <- liftIO $ atomically $ readTVar sest
  let active = Map.map blind a
  let pend   = Prelude.map blind p
  return $ State active pend
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
  ServantState{state=sest} <- ask
  res  <- liftIO $ atomically $ stateTVar sest $ \s@State{activeRequests=a,pendingRequests=p} ->
    case act of
      Complete ->
        (Right $ map snd $ findActive a, s{ activeRequests=(foldr (\k m -> Map.delete k m) a $ map fst $ findActive a)})
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
                    (Right matched, s{activeRequests=Map.insert (Responder name) x a, pendingRequests = rest})
                _ ->
                    (Left $ "Internal Error: more than one Request with ID " ++ (show id) ++ " found, wtf?", s)
      Discard  ->
        let (matched, rest) = split p in
        (Right matched, s{pendingRequests = rest})
  case res of
    Left err ->
      liftIO $ throwIO $ err400 { errBody = BS.fromString err }
    Right reqs ->
      return reqs
  where
    matchesid req = id == (reqid $ snd req)
    findActive = filter matchesid . Map.assocs
    split      = partition (\e -> reqid e == id)
handleAdminRequest _ _  = liftIO $ throwIO $ err400 { errBody = "Invalid user" }
          
