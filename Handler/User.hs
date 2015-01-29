module Handler.User where

import Import
import Helper
import Handler.BlogPost (blogPostList)

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId
    blogPosts <- selectBlogPostsByUser userId
    defaultLayout $(widgetFile "users/show")
