
{-# LANGUAGE OverloadedStrings #-}

module Forums.Handler
where
import Forums.View
import Forums.Found
import Forums.Model

import Develop.DatFw
import Develop.DatFw.Form
import Develop.DatFw.Form.Fields

import Data.Text as T

-- ---------------------------------------------------------------

markdownField :: Field (HandlerFor ForumsApp) Markdown
markdownField = checkMap
        (\ t -> if T.length t < minPostLen then Left "Text massa curt"
                else if T.length t > maxPostLen then Left "Text massa llarg"
                else Right (Markdown t))
        getMdText
        textareaField

-- ---------------------------------------------------------------
-- Controller handlers: Home

newForumForm :: AForm (HandlerFor ForumsApp) NewForum
newForumForm =
    NewForum <$> freq textField (withPlaceholder "Introduiu el títol del fòrum" "Titol") Nothing
             <*> freq markdownField (withPlaceholder "Introduiu la descripció del fòrum"
             "Descripció") Nothing
             
newTopicForm :: AForm (HandlerFor ForumsApp) NewTopic
newTopicForm =
    NewTopic <$> freq textField (withPlaceholder "Introduce topic title" "Title") Nothing
             <*> freq markdownField (withPlaceholder "Introduce topic description"
             "Description") Nothing

newReplyForm :: AForm (HandlerFor ForumsApp) Markdown
newReplyForm = freq markdownField (withPlaceholder "Introduce reply" "Reply") Nothing

getHomeR :: HandlerFor ForumsApp Html
getHomeR = do
    -- Get authenticated user
    mbuser <- maybeAuth
    -- Get a fresh form
    fformw <- generateAFormPost newForumForm
    -- Return HTML content
    appLayout $ homeView mbuser fformw

postHomeR :: HandlerFor ForumsApp Html
postHomeR = do
    user <- requireAuth
    (fformr, fformw) <- runAFormPost newForumForm
    case fformr of
        FormSuccess newtheme -> do
            runDbAction $ addForum (fst user) newtheme
            redirect HomeR
        _ ->
            appLayout $ homeView (Just user) fformw

-- ---------------------------------------------------------------
-- Controller handlers: Forum

getForumR :: ForumId -> HandlerFor ForumsApp Html
getForumR fid = do
    -- Get requested forum from data-base.
    -- responds with a 'Not found' status if forum don't exist
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    mbuser <- maybeAuth
    -- Other processing (forms, ...)
    tformw <- generateAFormPost newTopicForm
    -- Return HTML content
    appLayout $ forumView mbuser (fid, forum) tformw

postForumR :: ForumId -> HandlerFor ForumsApp Html
postForumR fid = do
    user <- requireAuth
    forum <- runDbAction (getForum fid) >>= maybe notFound pure
    (tformr,tformw) <- runAFormPost newTopicForm
    case tformr of
        FormSuccess newtopic -> do
            runDbAction $ addTopic fid (fst user) newtopic
            redirect (ForumR fid)
        _ ->
            appLayout $ forumView (Just user) (fid,forum) tformw

-- ---------------------------------------------------------------
-- Controller handlers: Topic

getTopicR :: TopicId -> HandlerFor ForumsApp Html
getTopicR tid = do
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    mbuser <- maybeAuth
    pformw <- generateAFormPost newReplyForm
    appLayout $ topicView mbuser (tid, topic) pformw

postTopicR :: TopicId -> HandlerFor ForumsApp Html
postTopicR tid = do
    user <- requireAuth
    topic <- runDbAction (getTopic tid) >>= maybe notFound pure
    (pformr, pformw) <- runAFormPost newReplyForm
    case pformr of
        FormSuccess newreply -> do
            runDbAction $ addReply (tdForumId topic) tid (fst user) newreply
            redirect (TopicR tid)
        _ ->
            appLayout $ topicView (Just user) (tid, topic) pformw


-- ---------------------------------------------------------------
-- Controller handlers: Autenticació

loginForm :: MonadHandler m => AForm m (Text, Text)
loginForm =
    (,) <$> freq textField "Nom d'usuari" Nothing
        <*> freq passwordField "Clau d'accés" Nothing

getLoginR :: HandlerFor ForumsApp Html
getLoginR = do
    setUltDestReferer
    -- Return HTML page
    (_, formw) <- runAFormPost loginForm
    appLayout $ loginView formw

postLoginR :: HandlerFor ForumsApp Html
postLoginR = do
    (formr, formw) <- runAFormPost loginForm
    case formr of
        FormSuccess (name, password) -> do
            ok <- validatePassword name password
            if ok then do
                -- Good credentials
                Just uid <- runDbAction $ loginUser name
                setSession authId_SESSION_KEY $ toPathPiece uid
                redirectUltDest HomeR
            else do
                -- Login error
                setMessage "Error d'autenticaciò"
                redirect LoginR
        _ ->
            appLayout (loginView formw)
    where
        validatePassword :: Text -> Text -> HandlerFor ForumsApp Bool
        validatePassword name password = do
            mbuser <- runDbAction $ getUserByName name
            case mbuser of
                Nothing -> pure False
                Just (_, user) -> pure $ pHashValidate password $ udPassword user

handleLogoutR :: HandlerFor ForumsApp ()
handleLogoutR = do
    -- | After logout (from the browser), redirect to the referring page.
    setUltDestReferer
    deleteSession authId_SESSION_KEY
    redirectUltDest HomeR

