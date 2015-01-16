module Handler.BlogPost where

import Import
import Yesod.Text.Markdown
import Yesod.Form.Bootstrap3

blogPostList :: [Entity BlogPost] -> Widget
blogPostList blogPosts = $(widgetFile "posts/list")

blogPostForm :: Maybe BlogPost -> Form BlogPost
blogPostForm mpost = renderBootstrap3 formLayout $ BlogPost
    <$> areq textField     (bfs ("Заголовок"  :: Text)) (blogPostTitle   <$> mpost)
    <*> areq markdownField (bfs ("Содержание" :: Text)) (blogPostContent <$> mpost)
    <*> maybe (lift now) (pure . blogPostCreated) mpost
  where
    now        = liftIO getCurrentTime
    formLayout = BootstrapBasicForm

getBlogPostsR :: Handler Html
getBlogPostsR = do
    blogPosts <- runDB $ selectList [] [Desc BlogPostCreated]
    defaultLayout $(widgetFile "posts/index")

postBlogPostsR :: Handler Html
postBlogPostsR = do
    ((res, blogPostWidget), enctype) <- runFormPost $ blogPostForm Nothing
    case res of
        FormSuccess blogPost -> do
            blogPostId <- runDB $ insert blogPost
            setMessage "Post was added"
            redirect $ BlogPostR blogPostId
        _ -> defaultLayout $(widgetFile "posts/new")

getNewBlogPostR :: Handler Html
getNewBlogPostR = do
    (blogPostWidget, enctype) <- generateFormPost $ blogPostForm Nothing
    defaultLayout $(widgetFile "posts/new")

getBlogPostR :: BlogPostId -> Handler Html
getBlogPostR blogPostId = do
    blogPost <- runDB $ get404 blogPostId
    defaultLayout $(widgetFile "posts/show")

patchBlogPostR :: BlogPostId -> Handler Html
patchBlogPostR blogPostId = do
    blogPost <- runDB $ get404 blogPostId
    ((res, blogPostWidget), enctype) <- runFormPost . blogPostForm $ Just blogPost
    case res of
        FormSuccess blogPost' -> do
            runDB $ replace blogPostId $ blogPost'
            setMessage "Post was updated"
            redirect $ BlogPostR blogPostId
        _ -> defaultLayout $(widgetFile "posts/edit")

getEditBlogPostR :: BlogPostId -> Handler Html
getEditBlogPostR blogPostId = do
    blogPost <- runDB $ get404 blogPostId
    (blogPostWidget, enctype) <- generateFormPost . blogPostForm $ Just blogPost
    defaultLayout $(widgetFile "posts/edit")

deleteBlogPostR :: BlogPostId -> Handler Html
deleteBlogPostR blogPostId = do
    runDB $ get404 blogPostId >> delete blogPostId
    redirect BlogPostsR
