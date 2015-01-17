module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)

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

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120
        "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage

        pc <- widgetToPageContent $ do
            addStylesheetRemote "http://fonts.googleapis.com/css?family=PT+Sans:400,700&subset=cyrillic,latin"
            addStylesheetRemote "http://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700&subset=latin,cyrillic"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css"
            addStylesheet $ StaticR css_default_css

            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
