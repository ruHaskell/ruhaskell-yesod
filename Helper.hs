module Helper where

import Import
import Database.Persist.Sql (rawSql)

-- isUniq :: Text -> EntityField v a -> Maybe a -> a -> Handler (Either Text a)
isUniq errorMessage field mexclude value = do
    count' <- runDB . count $ [field ==. value] ++ exclude
    return $ if count' > 0
             then Left errorMessage
             else Right value
  where
    exclude = maybe [] (\x -> [field !=. x]) mexclude

selectBlogPostsWithCategories :: Handler [(Entity BlogPost, Maybe (Entity Category))]
selectBlogPostsWithCategories = runDB $ rawSql sql []
  where
    sql =  "select ??, ?? from blog_post \
            left join category on blog_post.category_id = category.id \
            order by blog_post.created desc"

selectBlogPostsByCategory :: CategoryId -> Handler [(Entity BlogPost, Maybe (Entity Category))]
selectBlogPostsByCategory categoryId = runDB $ rawSql sql (keyToValues categoryId)
  where
    sql =  "select ??, ?? from blog_post \
            inner join category on blog_post.category_id = category.id \
            where category.id = ? \
            order by blog_post.created desc"

selectBlogPostsByTag :: TagId -> Handler [(Entity BlogPost, Maybe (Entity Category))]
selectBlogPostsByTag tagId = runDB $ rawSql sql (keyToValues tagId)
  where
    sql = "select ??, ?? from blog_post \
           left join category on blog_post.category_id = category.id \
           inner join blog_post_tag on blog_post.id = blog_post_tag.blog_post_id \
           where blog_post_tag.tag_id = ? \
           order by blog_post.created desc"
