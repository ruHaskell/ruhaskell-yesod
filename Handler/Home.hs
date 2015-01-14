module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    blogPosts <- runDB $ selectList [] [Desc BlogPostCreated, LimitTo 6]
    defaultLayout $(widgetFile "home")
