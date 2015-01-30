--{-# LANGUAGE AllowAmbiguousTypes #-}
module Handler.BlogPost where

import Import
import Helper
import Yesod.Text.Markdown
import Yesod.Form.Bootstrap3

isAdmin :: Handler Bool
isAdmin = do
    mauth <- maybeAuth
    return $ maybe False (userAdmin . entityVal) mauth

getBlogPostWithTags :: BlogPostId -> Handler (BlogPost, [TagId])
getBlogPostWithTags blogPostId = do
    blogPost <- runDB $ get404 blogPostId
    tags <- selectTagsByBlogPost blogPostId
    let tagIds = [entityKey t | t <- tags]
    return (blogPost, tagIds)

blogPostList :: [(Entity BlogPost, Maybe (Entity Category))] -> Widget
blogPostList blogPostsAndCategories = $(widgetFile "posts/list")

blogPostAForm :: Bool -> Maybe BlogPost -> AForm Handler BlogPost
blogPostAForm admin mpost = BlogPost
    <$> authorField admin
    <*> aopt categoriesList (bfs MsgBlogPostCategory) (blogPostCategoryId <$> mpost)
    <*> areq textField      (bfs MsgBlogPostTitle)    (blogPostTitle      <$> mpost)
    <*> areq markdownField  (bfs MsgBlogPostContent)  (blogPostContent    <$> mpost)
    <*> createdField admin
    <*> areq checkBoxField  (bfs MsgBlogPostDraft)    (blogPostDraft      <$> mpost)
  where
    authorField True  = areq (selectField users) (bfs MsgBlogPostUser) (blogPostAuthorId <$> mpost)
    authorField False = lift requireAuthId

    createdField True  = areq utcField (bfs MsgBlogPostDate) (blogPostCreated <$> mpost)
    createdField False = maybe (lift now) (pure . blogPostCreated) mpost

    users :: Handler (OptionList UserId)
    users = do
        entities <- runDB $ selectList [] [Asc UserName]
        optionsPairs [(userName val, userId) | Entity userId val <- entities]
    categoriesList = selectField categories
    categories :: Handler (OptionList CategoryId)
    categories = do
        entities <- runDB $ selectList [] [Asc CategoryTitle]
        optionsPairs [(categoryTitle val, categoryId) | Entity categoryId val <- entities]
    now = liftIO getCurrentTime

tagsAForm :: Maybe [TagId] -> AForm Handler (Maybe [TagId])
tagsAForm mvalues = aopt (checkboxesField tags) fs (Just mvalues)
  where
    fs = FieldSettings (SomeMessage MsgBlogPostTags) Nothing (Just "blog_post_tags") Nothing []
    tags :: Handler (OptionList TagId)
    tags = do
        entities <- runDB $ selectList [] [Asc TagTitle]
        optionsPairs [(tagTitle val, tagId) | Entity tagId val <- entities]

blogPostForm :: Bool -> Maybe (BlogPost, [TagId]) -> Form (BlogPost, [TagId])
blogPostForm admin mpair = renderBootstrap3 BootstrapBasicForm $ (,)
                           <$> blogPostAForm admin mpost
                           <*> fmap (fromMaybe []) (tagsAForm mvalues)
  where
    mpost   = fst <$> mpair
    mvalues = snd <$> mpair

getBlogPostsR :: Handler Html
getBlogPostsR = do
    blogPosts <- selectBlogPostsWithCategories
    defaultLayout $(widgetFile "posts/index")

postBlogPostsR :: Handler Html
postBlogPostsR = do
    admin <- isAdmin
    ((res, blogPostWidget), enctype) <- runFormPost $ blogPostForm admin Nothing
    case res of
        FormSuccess (blogPost, tagIds) -> do
            blogPostId <- runDB $ do
                blogPostId <- insert blogPost
                insertMany $ map (BlogPostTag blogPostId) tagIds
                return blogPostId
            setMessageI MsgBlogPostWasAdded
            redirect $ BlogPostR blogPostId
        _ -> defaultLayout $(widgetFile "posts/new")

getNewBlogPostR :: Handler Html
getNewBlogPostR = do
    admin <- isAdmin
    (blogPostWidget, enctype) <- generateFormPost $ blogPostForm admin Nothing
    defaultLayout $(widgetFile "posts/new")

getBlogPostR :: BlogPostId -> Handler Html
getBlogPostR blogPostId = do
    mauth <- maybeAuth
    (blogPost, author, category)  <- runDB $ do
        blogPost <- get404 blogPostId
        author <- get404 $ blogPostAuthorId blogPost
        category <- case blogPostCategoryId blogPost of
                         Just categoryId -> selectFirst [CategoryId ==. categoryId] []
                         Nothing -> return Nothing
        return (blogPost, author, category)
    tags <- selectTagsByBlogPost blogPostId
    defaultLayout $(widgetFile "posts/show")

patchBlogPostR :: BlogPostId -> Handler Html
patchBlogPostR blogPostId = do
    admin <- isAdmin
    (blogPost, tagIds) <- getBlogPostWithTags blogPostId
    ((res, blogPostWidget), enctype) <- runFormPost $ blogPostForm admin $ Just (blogPost, tagIds)
    case res of
        FormSuccess (blogPost', tagIds') -> do
            runDB $ do
                replace blogPostId blogPost'
                deleteWhere [BlogPostTagBlogPostId ==. blogPostId]
                insertMany $ map (BlogPostTag blogPostId) tagIds'
            setMessageI MsgBlogPostWasUpdated
            redirect $ BlogPostR blogPostId
        _ -> defaultLayout $(widgetFile "posts/edit")

getEditBlogPostR :: BlogPostId -> Handler Html
getEditBlogPostR blogPostId = do
    admin <- isAdmin
    (blogPost, tagIds) <- getBlogPostWithTags blogPostId
    (blogPostWidget, enctype) <- generateFormPost $ blogPostForm admin $ Just (blogPost, tagIds)
    defaultLayout $(widgetFile "posts/edit")

deleteBlogPostR :: BlogPostId -> Handler Html
deleteBlogPostR blogPostId = do
    runDB $ get404 blogPostId >> delete blogPostId
    redirect BlogPostsR
