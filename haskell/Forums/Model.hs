
{-# LANGUAGE OverloadedStrings #-}

module Forums.Model
    ( module Forums.Model
    , Connection, openDb, closeDb
    , runDbTran, DbM, Key(..)
    )
where
import Db

import Data.Text as T
import Data.Monoid
import Data.Int
import Data.Time
import Crypto.KDF.BCrypt as BCrypt
import Data.ByteString as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.SQLite.Simple.FromField as SQL
import Control.Monad.IO.Class
import Text.Blaze (ToMarkup(..))
import CMark

-- ---------------------------------------------------------------
-- Basic types
-- ---------------------------------------------------------------

minTitleLen, maxTitleLen, minPostLen, maxPostLen :: Int
minTitleLen = 8
maxTitleLen = 100
minPostLen = 20
maxPostLen = 10000

-- ---------------------------------------------------------------
-- Markdown

newtype Markdown = Markdown { getMdText :: Text }
        deriving (Show)

instance ToMarkup Markdown where
    toMarkup (Markdown t) = preEscapedToMarkup $ commonmarkToHtml [optSafe] t


instance FromField Markdown where
  fromField f = Markdown <$> fromField f

instance ToField Markdown where
  toField (Markdown t) = toField t

-- ---------------------------------------------------------------
-- Password (clear text)

data PasswordHash = PHashBCrypt ByteString
                  | PHashClear Text
                  | PHashBlocked
        deriving (Show)

pHashMkClear :: Text -> PasswordHash
pHashMkClear p = PHashClear p

pHashBCrypt :: Text -> IO PasswordHash
pHashBCrypt p = do
    h <- BCrypt.hashPassword 10 (encodeUtf8 p)
    pure $ PHashBCrypt h

pHashValidate :: Text -> PasswordHash -> Bool
pHashValidate p (PHashBCrypt h) =
    BCrypt.validatePassword (encodeUtf8 p) h
pHashValidate p1 (PHashClear p2) =
    p1 == p2
pHashValidate _ PHashBlocked =
    False

instance FromField PasswordHash where
  fromField f = do -- Monad Ok
      (pre, rem) <- T.break (':' ==) <$> fromField f
      if T.null rem
          then returnError ConversionFailed f "Invalid format (no prefix)"
          else do
            let t = T.tail rem
            case pre of
                "bcrypt" -> pure $ PHashBCrypt $ encodeUtf8 t
                "clear" -> pure $ PHashClear t
                "blocked" -> pure PHashBlocked
                _ -> returnError ConversionFailed f $ "Invalid prefix " <> show pre

instance ToField PasswordHash where
  toField (PHashBCrypt h) = toField $ "bcrypt:" <> decodeUtf8 h
  toField (PHashClear p) = toField $ "clear:" <> p
  toField PHashBlocked = toField ("blocked:" :: Text)


-- ---------------------------------------------------------------
-- Entity types
-- ---------------------------------------------------------------

-- ---------------------------------------------------------------
-- Table: UserD

type UserId = Key UserD

data UserD = UserD
        { udName :: Text
        , udPassword :: PasswordHash
        , udLoginCount :: Int
        , udLastLogin :: Maybe UTCTime
        , udIsAdmin :: Bool
        }
        deriving (Show)

instance DbEntity UserD where
    tableName _ = "users"
    columnNames _ = ["name","password", "loginCount", "lastLogin", "isAdmin"]

instance FromRow UserD where
  fromRow = UserD <$> field <*> field <*> field <*> field <*> field

instance ToRow UserD where
  toRow (UserD f1 f2 f3 f4 f5) = toRow (f1, f2, f3, f4, f5)


getUser :: UserId -> DbM (Maybe UserD)
getUser uid = do
    get uid

getUserByName :: Text -> DbM (Maybe (UserId, UserD))
getUserByName name = do
    us <- select (\ _ u -> name == udName u)
    case us of
        u : _ -> pure $ Just u
        [] -> pure Nothing

loginUser :: Text -> DbM (Maybe UserId)
loginUser name = do
    mbu <- getUserByName name
    case mbu of
        Just (uid, u) -> do
            now <- liftIO getCurrentTime
            ph <- liftIO $ protectPass (udPassword u)
            let u2 = u{ udLoginCount = udLoginCount u + 1, udLastLogin = Just now, udPassword = ph }
            set uid u2
            pure (Just uid)
        Nothing ->
            pure Nothing
    where
        protectPass :: PasswordHash -> IO PasswordHash
        protectPass (PHashClear t) = pHashBCrypt t
        protectPass p              = pure p

changeUserPassword :: UserId -> Text -> DbM ()
changeUserPassword uid pw = do
    mbu <- getUser uid
    case mbu of
        Just u -> do
            ph <- liftIO $ pHashBCrypt pw
            set uid u{ udPassword = ph }
        Nothing ->
            fail $ "User with id " <> show uid <> " don't exist"


-- ---------------------------------------------------------------
-- Table: ForumD

type ForumId = Key ForumD

data ForumD = ForumD
        { fdCategory :: Text
        , fdTitle :: Text
        , fdDescription :: Markdown
        , fdModeratorId :: UserId
        , fdCreated :: UTCTime         -- Forum creation time

        , fdTopicCount :: Int
        , fdPostCount :: Int
        , fdLastPostId :: Maybe PostId
        }
        deriving (Show)

instance DbEntity ForumD where
    tableName _ = "forums"
    columnNames _ = ["category","title","description","moderatorId","created"
                    ,"topicCount","postCount","lastPostId"
                    ]

instance FromRow ForumD where
  fromRow = ForumD <$> field <*> field <*> field <*> field <*> field
                  <*> field <*> field <*> field

instance ToRow ForumD where
  toRow (ForumD f1 f2 f3 f4 f5 f6 f7 f8) = toRow (f1, f2, f3, f4, f5, f6, f7, f8)


getForumList :: DbM [(ForumId, ForumD)]
getForumList =
    select (const $ const True)

getForum :: ForumId -> DbM (Maybe ForumD)
getForum fid = do
    get fid

data NewForum = NewForum { nfTitle :: Text, nfDescription :: Markdown }

addForum :: UserId -> NewForum -> DbM ForumId
addForum uid nf = do
    now <- liftIO getCurrentTime
    add $ ForumD { fdCategory = ""
                 , fdTitle = nfTitle nf
                 , fdDescription = nfDescription nf
                 , fdModeratorId = uid
                 , fdCreated = now
                 , fdTopicCount = 0
                 , fdPostCount = 0
                 , fdLastPostId = Nothing
                 }

editForum :: ForumId -> Text -> Markdown -> DbM ()
editForum fid title description =
    -- update :: ForumId -> (ForumD -> ForumD) -> DbM ()
    update fid $ \ forumD -> forumD{ fdTitle = title, fdDescription = description }

deleteForum :: ForumId -> DbM ()
deleteForum fid = do
    ts <- getTopicList fid
    mapM_ (deleteTopic fid) (fst <$> ts)
    Db.delete fid


-- ---------------------------------------------------------------
-- Table: TopicD (aka thread)

type TopicId = Key TopicD

data TopicD = TopicD
        { tdForumId :: ForumId
        , tdSubject :: Text
        , tdUserId :: UserId
        , tdStarted :: UTCTime

        , tdPostCount :: Int
        , tdFirstPostId :: Maybe PostId
        , tdLastPostId :: Maybe PostId
        ---, tdLastPostNum :: Int           -- 0 for the first post (the topic starter)
        }
        deriving (Show)

instance DbEntity TopicD where
    tableName _ = "topics"
    columnNames _ = ["forumId","subject","userId","started","postCount","firstPostId","lastPostId"]

instance FromRow TopicD where
  fromRow = TopicD <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow TopicD where
  toRow (TopicD f1 f2 f3 f4 f5 f6 f7) = toRow (f1, f2, f3, f4, f5, f6, f7)

getTopicList :: ForumId -> DbM [(TopicId, TopicD)]
getTopicList fid =
    ---selectOrder (const $ (fid==) . tdForumId) tdLastPosted
    select (const $ (fid==) . tdForumId)

getTopic :: TopicId -> DbM (Maybe TopicD)
getTopic tid = do
    get tid

data NewTopic = NewTopic { ntSubject :: Text, ntMessage :: Markdown }

addTopic :: ForumId -> UserId -> NewTopic -> DbM (TopicId, PostId)
addTopic fid uid nt = do
    now <- liftIO getCurrentTime
    -- Create the new topic and get its identifier
    let topic0 = TopicD
                { tdForumId = fid
                , tdSubject = ntSubject nt
                , tdUserId = uid
                , tdStarted = now
                , tdPostCount = 0
                , tdFirstPostId = Nothing
                , tdLastPostId = Nothing
                }
    tid <- add topic0
    -- Create the topic's first post (the question) and get its identifier
    pid <- add $ PostD
                { pdTopicId = tid
                , pdUserId = uid
                , pdPosted = now
                , pdMessage = ntMessage nt
                }
    -- Set the topic's last post to the identifier of the created post
    set tid $ topic0{ tdPostCount = 1
                    , tdFirstPostId = Just pid
                    , tdLastPostId = Just pid
                    }
    -- Update the forum's summary information
    update fid $
        \ forum -> forum{ fdTopicCount = fdTopicCount forum + 1
                        , fdPostCount = fdPostCount forum + 1
                        , fdLastPostId = Just pid
                        }
    pure (tid, pid)

deleteTopic :: ForumId -> TopicId -> DbM ()
deleteTopic fid tid = do
    posts <- getPostList tid
    mapM_ (deletePost fid tid) (fst <$> posts)
    update fid $ \ forum -> forum{ fdTopicCount = fdTopicCount forum - 1 }
    Db.delete tid


-- ---------------------------------------------------------------
-- Table: PostD

type PostId = Key PostD

data PostD = PostD
        { pdTopicId :: TopicId
        , pdUserId :: UserId
        , pdPosted :: UTCTime
        ---, pdNum :: Int                   -- 0 for the first post (the topic starter)
        , pdMessage :: Markdown
        }
        deriving (Show)

instance DbEntity PostD where
    tableName _ = "posts"
    columnNames _ = ["topicId","userId","posted","message"]

instance FromRow PostD where
  fromRow = PostD <$> field <*> field <*> field <*> field

instance ToRow PostD where
  toRow (PostD f1 f2 f3 f4) = toRow (f1, f2, f3, f4)

getPostList :: TopicId -> DbM [(PostId, PostD)]
getPostList tid =
    selectOrder (const $ (tid==) . pdTopicId) pdPosted

getPost :: PostId -> DbM (Maybe PostD)
getPost pid = do
    get pid

addReply :: ForumId -> TopicId -> UserId -> Markdown -> DbM PostId
addReply fid tid uid newtext = do
    -- Create the new post
    now <- liftIO getCurrentTime
    pid <- add $ PostD tid uid now newtext
    -- Update the topic's summary information
    update tid $
        \ topic -> topic{ tdPostCount = tdPostCount topic + 1
                        , tdLastPostId = Just pid
                        }
    -- Update the forum's summary information
    update fid $
        \ forum -> forum{ fdPostCount = fdPostCount forum + 1
                        , fdLastPostId = Just pid
                        }
    pure pid

deletePost :: ForumId -> TopicId -> PostId -> DbM ()
deletePost fid tid pid = do
    update fid $
        \ forum -> forum{ fdPostCount = fdPostCount forum - 1
                        , fdLastPostId = if Just pid == fdLastPostId forum then Nothing else fdLastPostId forum
                        }
    update tid $
        \ topic -> topic{ tdPostCount = tdPostCount topic - 1
                        , tdFirstPostId = if Just pid == tdFirstPostId topic then Nothing else tdFirstPostId topic
                        , tdLastPostId = if Just pid == tdLastPostId topic then Nothing else tdLastPostId topic
                        }
    Db.delete pid

