module Handler.Home where

import Import
import Helper
import Handler.BlogPost (blogPostList)

getHomeR :: Handler Html
getHomeR = do
    -- TODO: change it to limit version
    blogPosts <- selectBlogPostsWithCategories
    defaultLayout $(widgetFile "home")
