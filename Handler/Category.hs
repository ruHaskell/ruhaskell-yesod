module Handler.Category where

import Import
import Helper
import Yesod.Form.Bootstrap3
import Handler.BlogPost (blogPostList)

categoryForm :: Maybe Category -> Form Category
categoryForm mcat = renderBootstrap3 BootstrapBasicForm $ Category
                    <$> areq titleField (bfs MsgCategoryTitle) mtitle
  where
    mtitle     = categoryTitle <$> mcat
    titleField = checkM (isUniq MsgCategoryAlreadyExists CategoryTitle mtitle) textField

getCategoriesR :: Handler Html
getCategoriesR = do
    categories <- selectCategories
    defaultLayout $(widgetFile "categories/index")

postCategoriesR :: Handler Html
postCategoriesR = do
    ((res, formWidget), enctype) <- runFormPost $ categoryForm Nothing
    case res of
        FormSuccess category -> do
            _ <- runDB $ insert category
            setMessageI MsgCategoryWasAdded
            redirect CategoriesR
        _ -> defaultLayout $(widgetFile "categories/new")

getNewCategoryR :: Handler Html
getNewCategoryR = do
    (formWidget, enctype) <- generateFormPost $ categoryForm Nothing
    defaultLayout $(widgetFile "categories/new")

getCategoryR :: CategoryId -> Handler Html
getCategoryR categoryId = do
    mauth <- maybeAuth
    category <- runDB $ get404 categoryId
    blogPosts <- selectBlogPostsByCategory categoryId
    defaultLayout $(widgetFile "categories/show")

patchCategoryR :: CategoryId -> Handler Html
patchCategoryR categoryId = do
    category <- runDB $ get404 categoryId
    ((res, formWidget), enctype) <- runFormPost . categoryForm $ Just category
    case res of
        FormSuccess category' -> do
            runDB $ replace categoryId $ category'
            setMessageI MsgCategoryWasUpdated
            redirect CategoriesR
        _ -> defaultLayout $(widgetFile "categories/edit")

getEditCategoryR :: CategoryId -> Handler Html
getEditCategoryR categoryId = do
    category <- runDB $ get404 categoryId
    (formWidget, enctype) <- generateFormPost . categoryForm $ Just category
    defaultLayout $(widgetFile "categories/edit")

deleteCategoryR :: CategoryId -> Handler Html
deleteCategoryR categoryId = do
    runDB $ get404 categoryId >> delete categoryId
    redirect CategoriesR
