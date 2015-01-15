module Handler.Home where

import Import
import Handler.BlogPost (blogPostList)

getHomeR :: Handler Html
getHomeR = do
    blogPosts <- runDB $ selectList [] [Desc BlogPostCreated, LimitTo 6]
    defaultLayout $(widgetFile "home")
