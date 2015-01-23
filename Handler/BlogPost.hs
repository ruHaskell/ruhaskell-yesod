module Handler.BlogPost where

import Import
import Helper
import Yesod.Text.Markdown
import Yesod.Form.Bootstrap3

getBlogPostWithTags :: BlogPostId -> Handler (BlogPost, [TagId])
getBlogPostWithTags blogPostId = do
    blogPost <- runDB $ get404 blogPostId
    tags <- selectTagsByBlogPost blogPostId
    let tagIds = [entityKey t | t <- tags]
    return (blogPost, tagIds)

blogPostList :: [(Entity BlogPost, Maybe (Entity Category))] -> Widget
blogPostList blogPostsAndCategories = $(widgetFile "posts/list")

blogPostAForm :: Maybe BlogPost -> AForm Handler BlogPost
blogPostAForm mpost = BlogPost
    <$> aopt categoriesList (bfs ("Категория"  :: Text)) (blogPostCategoryId <$> mpost)
    <*> areq textField      (bfs ("Заголовок"  :: Text)) (blogPostTitle      <$> mpost)
    <*> areq markdownField  (bfs ("Содержание" :: Text)) (blogPostContent    <$> mpost)
    <*> maybe (lift now) (pure . blogPostCreated) mpost
  where
    categoriesList = selectField categories
    categories :: Handler (OptionList CategoryId)
    categories = do
        entities <- runDB $ selectList [] [Asc CategoryTitle]
        optionsPairs [(categoryTitle val, categoryId) | Entity categoryId val <- entities]
    now = liftIO getCurrentTime

tagsAForm :: Maybe [TagId] -> AForm Handler (Maybe [TagId])
tagsAForm mvalues = aopt (checkboxesField tags) (fs "blog_post_tags" "Теги") (Just mvalues)
  where
    fs :: Text -> Text -> FieldSettings site
    fs fieldId msg = FieldSettings (SomeMessage msg) Nothing (Just fieldId) Nothing []
    tags :: Handler (OptionList TagId)
    tags = do
        entities <- runDB $ selectList [] [Asc TagTitle]
        optionsPairs [(tagTitle val, tagId) | Entity tagId val <- entities]

blogPostForm :: Maybe (BlogPost, [TagId]) -> Form (BlogPost, [TagId])
blogPostForm mpair = renderBootstrap3 BootstrapBasicForm $ (,)
                     <$> blogPostAForm mpost
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
    ((res, blogPostWidget), enctype) <- runFormPost $ blogPostForm Nothing
    case res of
        FormSuccess (blogPost, tagIds) -> do
            blogPostId <- runDB $ do
                blogPostId <- insert blogPost
                insertMany $ map (BlogPostTag blogPostId) tagIds
                return blogPostId
            setMessage "Post was added"
            redirect $ BlogPostR blogPostId
        _ -> defaultLayout $(widgetFile "posts/new")

getNewBlogPostR :: Handler Html
getNewBlogPostR = do
    (blogPostWidget, enctype) <- generateFormPost $ blogPostForm Nothing
    defaultLayout $(widgetFile "posts/new")

getBlogPostR :: BlogPostId -> Handler Html
getBlogPostR blogPostId = do
    (blogPost, category)  <- runDB $ do
        blogPost <- get404 blogPostId
        category <- case blogPostCategoryId blogPost of
                         Just categoryId -> selectFirst [CategoryId ==. categoryId] []
                         Nothing -> return Nothing
        return (blogPost, category)
    tags <- selectTagsByBlogPost blogPostId
    defaultLayout $(widgetFile "posts/show")

patchBlogPostR :: BlogPostId -> Handler Html
patchBlogPostR blogPostId = do
    (blogPost, tagIds) <- getBlogPostWithTags blogPostId
    ((res, blogPostWidget), enctype) <- runFormPost . blogPostForm $ Just (blogPost, tagIds)
    case res of
        FormSuccess (blogPost', tagIds') -> do
            runDB $ do
                replace blogPostId blogPost'
                deleteWhere [BlogPostTagBlogPostId ==. blogPostId]
                insertMany $ map (BlogPostTag blogPostId) tagIds'
            setMessage "Post was updated"
            redirect $ BlogPostR blogPostId
        _ -> defaultLayout $(widgetFile "posts/edit")

getEditBlogPostR :: BlogPostId -> Handler Html
getEditBlogPostR blogPostId = do
    (blogPost, tagIds) <- getBlogPostWithTags blogPostId
    (blogPostWidget, enctype) <- generateFormPost . blogPostForm $ Just (blogPost, tagIds)
    defaultLayout $(widgetFile "posts/edit")

deleteBlogPostR :: BlogPostId -> Handler Html
deleteBlogPostR blogPostId = do
    runDB $ get404 blogPostId >> delete blogPostId
    redirect BlogPostsR
