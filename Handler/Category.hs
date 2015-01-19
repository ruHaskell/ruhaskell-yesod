module Handler.Category where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Sql (Single(..), rawSql)
import Handler.BlogPost (blogPostList)

categoryForm :: Maybe Category -> Form Category
categoryForm mcat = renderBootstrap3 BootstrapBasicForm $ Category
                    <$> areq titleField (bfs ("Заголовок" :: Text)) (categoryTitle <$> mcat)
  where
    titleField   = checkM isUniq textField
    exclude      = maybe [] (\t -> [CategoryTitle !=. t]) (categoryTitle <$> mcat)
    isUniq title = do
        categoryCount <- runDB . count $ [CategoryTitle ==. title] ++ exclude
        return $ if categoryCount > 0
                 then Left ("Такая категория уже есть" :: Text)
                 else Right title

getCategoriesR :: Handler Html
getCategoriesR = do
    categories <- selectCategories
    defaultLayout $(widgetFile "categories/index")
  where
    sql = "select ??, count(distinct blog_post.id) from category\
           left join blog_post on category.id = blog_post.category group by category.id"

    selectCategories :: Handler [(Entity Category, Single Int)]
    selectCategories = runDB $ rawSql sql []

postCategoriesR :: Handler Html
postCategoriesR = do
    ((res, formWidget), enctype) <- runFormPost $ categoryForm Nothing
    case res of
        FormSuccess category -> do
            runDB $ insert category
            setMessage "Category was added"
            redirect CategoriesR
        _ -> defaultLayout $(widgetFile "categories/new")

getNewCategoryR :: Handler Html
getNewCategoryR = do
    (formWidget, enctype) <- generateFormPost $ categoryForm Nothing
    defaultLayout $(widgetFile "categories/new")

getCategoryR :: CategoryId -> Handler Html
getCategoryR categoryId = do
    category <- runDB $ get404 categoryId
    blogPosts <- runDB $ selectList [BlogPostCategory ==. Just categoryId] [Desc BlogPostCreated]
    defaultLayout $(widgetFile "categories/show")

patchCategoryR :: CategoryId -> Handler Html
patchCategoryR categoryId = do
    category <- runDB $ get404 categoryId
    ((res, formWidget), enctype) <- runFormPost . categoryForm $ Just category
    case res of
        FormSuccess category' -> do
            runDB $ replace categoryId $ category'
            setMessage "Category was updated"
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
