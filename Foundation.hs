module Foundation where

import Import.NoFoundation
import Database.Persist.Sql     (ConnectionPool, runSqlPool)
import Text.Hamlet              (hamletFile)
import Text.Jasmine             (minifym)
import Yesod.Auth.OAuth2.Github (oauth2Github)
import Yesod.Default.Util       (addStaticContentExternal)
import Yesod.Core.Types         (Logger)
import Yesod.Form.I18n.Russian  (russianFormMessage)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static
    , appConnPool    :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

mkYesodData "App" $(parseRoutesFile "config/routes")

mkMessage "App" "messages" "ru"

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120
        "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage
        ma <- maybeAuth

        pc <- widgetToPageContent $ do
            addStylesheetRemote "http://fonts.googleapis.com/css?family=PT+Sans:400,700&subset=cyrillic,latin"
            addStylesheetRemote "http://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700&subset=latin,cyrillic"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"
            addStylesheet $ StaticR css_default_css

            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR LoginR

    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR  _ = return Authorized
    isAuthorized _        _ = return Authorized

    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False

    getAuthId creds = runDB $ do
        let extra = credsExtra creds
        $(logDebug) $ "Extra account information: " <> (pack . show $ extra)

        let ident =
                 case lookup "login" extra of
                     Just login -> login
                     Nothing ->
                         case lookup "email" extra of
                             Just email -> email
                             Nothing -> credsIdent creds

        x <- getBy $ UniqueUser ident
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent    = ident
                    , userPassword = Nothing
                    }

    authPlugins app =
        let mkPlugin (OA2Provider{..}) =
                case (oa2provider, oa2clientId, oa2clientSecret) of
                    (_, _, "not-configured") -> Nothing
                    (_, "not-configured", _) -> Nothing

                    ("github", cid, sec) ->
                        Just $ oauth2Github (pack cid) (pack sec)

                    _ -> Nothing
        in catMaybes . map mkPlugin
                     . appOA2Providers
                     $ appSettings app

    authHttpManager = getHttpManager

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage _ _ = russianFormMessage
