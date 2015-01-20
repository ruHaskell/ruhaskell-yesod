module Handler.Tag where

import Import
import Helper
import Yesod.Form.Bootstrap3
import Database.Persist.Sql (Single(..), rawSql)
import Handler.BlogPost (blogPostList)

tagForm :: Maybe Tag -> Form Tag
tagForm mtag = renderBootstrap3 BootstrapBasicForm $ Tag
               <$> areq titleField (bfs ("Заголовок" :: Text)) mtitle
  where
    mtitle     = tagTitle <$> mtag
    titleField = checkM (isUniq ("Такой тег уже есть" :: Text) TagTitle mtitle) textField

getTagsR :: Handler Html
getTagsR = do
    tags <- selectTags
    defaultLayout $(widgetFile "tags/index")
  where
    sql = "select ??, count(distinct blog_post_tag.blog_post_id) from tag\
           left join blog_post_tag on tag.id = blog_post_tag.tag_id group by tag.id"

    selectTags :: Handler [(Entity Tag, Single Int)]
    selectTags = runDB $ rawSql sql []

postTagsR :: Handler Html
postTagsR = do
    ((res, formWidget), enctype) <- runFormPost $ tagForm Nothing
    case res of
        FormSuccess tag -> do
            _ <- runDB $ insert tag
            setMessage "Tag was added"
            redirect TagsR
        _ -> defaultLayout $(widgetFile "tags/new")

getNewTagR :: Handler Html
getNewTagR = do
    (formWidget, enctype) <- generateFormPost $ tagForm Nothing
    defaultLayout $(widgetFile "tags/new")

getTagR :: TagId -> Handler Html
getTagR tagId = do
    tag <- runDB $ get404 tagId
    blogPosts <- selectBlogPosts
    defaultLayout $(widgetFile "tags/show")
  where
    sql = "select ?? from blog_post \
           left join blog_post_tag on blog_post.id = blog_post_tag.blog_post_id \
           where blog_post_tag.tag_id = ? \
           order by blog_post.created desc"

    selectBlogPosts :: Handler [Entity BlogPost]
    selectBlogPosts = runDB $ rawSql sql (keyToValues tagId)

patchTagR :: TagId -> Handler Html
patchTagR tagId = do
    tag <- runDB $ get404 tagId
    ((res, formWidget), enctype) <- runFormPost . tagForm $ Just tag
    case res of
        FormSuccess tag' -> do
            runDB $ replace tagId $ tag'
            setMessage "Tag was updated"
            redirect TagsR
        _ -> defaultLayout $(widgetFile "tags/edit")

getEditTagR :: TagId -> Handler Html
getEditTagR tagId = do
    tag <- runDB $ get404 tagId
    (formWidget, enctype) <- generateFormPost . tagForm $ Just tag
    defaultLayout $(widgetFile "tags/edit")

deleteTagR :: TagId -> Handler Html
deleteTagR tagId = do
    runDB $ get404 tagId >> delete tagId
    redirect TagsR
