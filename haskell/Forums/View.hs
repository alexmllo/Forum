
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Forums.View
where
import           Forums.Config
import           Forums.Found
import           Forums.Model

import           Develop.DatFw
import           Develop.DatFw.Widget
import           Develop.DatFw.Template

import           Control.Monad.IO.Class   -- imports liftIO
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Time
import           Data.Semigroup
import           Text.Blaze
import           Language.Haskell.TH.Syntax

-- ---------------------------------------------------------------
-- Utilities for the view components
-- ---------------------------------------------------------------

uidNameWidget :: UserId -> Widget ForumsApp
uidNameWidget uid = do
    uname <- maybe "???" udName <$> runDbAction (getUser uid)
    toWidget $ toMarkup uname

dateWidget :: UTCTime -> Widget ForumsApp
dateWidget time = do
    zt <- liftIO $ utcToLocalZonedTime time
    let locale = defaultTimeLocale
    toWidget $ toMarkup $ T.pack $ formatTime locale "%e %b %Y, %H:%M" zt

pidPostedWidget :: PostId -> Widget ForumsApp
pidPostedWidget pid = do
    mbpost <- runDbAction $ getPost pid
    maybe "???" (dateWidget . pdPosted) mbpost


-- ---------------------------------------------------------------
-- Views
-- ---------------------------------------------------------------

homeView :: Maybe (UserId, UserD) -> Widget ForumsApp -> Widget ForumsApp
homeView mbuser fformw = do
    forums <- runDbAction getForumList
    $(widgetTemplFile $ templatesDir <> "/home.html")

forumView :: Maybe (UserId, UserD) -> (ForumId, ForumD) -> Widget ForumsApp -> WidgetFor ForumsApp ()
forumView mbuser (fid, forum) tformw = do
    topics <- runDbAction $ getTopicList fid
    $(widgetTemplFile $ templatesDir <> "/forum.html")
    
topicView :: Maybe (UserId, UserD) -> (TopicId,TopicD) -> Widget ForumsApp -> WidgetFor ForumsApp ()
topicView mbuser (tid, topic) pformw = do
    replies <- runDbAction $ getPostList tid
    $(widgetTemplFile $ templatesDir <> "/topic.html")

loginView :: Widget ForumsApp -> Widget ForumsApp
loginView formw =
    $(widgetTemplFile $ templatesDir <> "/login.html")


-- ---------------------------------------------------------------
-- Application Page Layout

appLayout :: Widget ForumsApp -> HandlerFor ForumsApp Html
appLayout wdgt = do
    page <- widgetToPageContent wdgt
    mbmsg <- getMessage
    mbuser <- fmap (fmap snd) maybeAuth
    applyUrlRenderTo $(htmlTemplFile $ templatesDir <> "/default-layout.html")

