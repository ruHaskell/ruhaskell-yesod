module Handler.User where

import Import
import Helper
import Handler.BlogPost (blogPostList)

getUsersR :: Handler Html
getUsersR = do
    users <- selectUsers
    defaultLayout $(widgetFile "users/index")

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId
    blogPosts <- selectBlogPostsByUser userId
    defaultLayout $(widgetFile "users/show")
