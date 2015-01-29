module Helper where

import Import
import Database.Persist.Sql (Single(..), rawSql)
import Text.Printf (printf)

allowedToWriteBlogPost :: UserId -> User -> BlogPost -> Bool
allowedToWriteBlogPost userId user blogPost = userAdmin user || userId == blogPostAuthorId blogPost

renderDateAsTuple :: UTCTime -> String
renderDateAsTuple time =
    case toGregorian . utctDay $ time of
        (y, m, d) -> printf "fromGregorian %d %d %d" y m d

withRemaining :: [a] -> [(a, Int)]
withRemaining xs = zip xs (reverse (enumFromTo 0 (length xs - 1)))

-- isUniq :: Text -> EntityField v a -> Maybe a -> a -> Handler (Either Text a)
isUniq errorMessage field mexclude value = do
    count' <- runDB . count $ [field ==. value] ++ exclude
    return $ if count' > 0
             then Left errorMessage
             else Right value
  where
    exclude = maybe [] (\x -> [field !=. x]) mexclude

selectCategories :: Handler [(Entity Category, Int)]
selectCategories = do
    categories <- runDB $ rawSql sql []
    return [(category, count') | (category, Single count') <- categories]
  where
    sql = "select ??, count(blog_post.id) from category \
           left join blog_post on category.id = blog_post.category_id \
           group by category.id"

selectTags :: Handler [(Entity Tag, Int)]
selectTags = do
    tags <- runDB $ rawSql sql []
    return [(tag, count') | (tag, Single count') <- tags]
  where
    sql = "select ??, count(blog_post_tag.blog_post_id) from tag \
           left join blog_post_tag on tag.id = blog_post_tag.tag_id \
           group by tag.id"

selectUsers :: Handler [(Entity User, Int)]
selectUsers = do
    users <- runDB $ rawSql sql []
    return [(user, count') | (user, Single count') <- users]
  where
    sql = "select ??, count(blog_post.author_id) from \"user\" \
           left join blog_post on \"user\".id = blog_post.author_id \
           group by \"user\".id"

selectTagsByBlogPost :: BlogPostId -> Handler [Entity Tag]
selectTagsByBlogPost blogPostId = runDB $ rawSql sql (keyToValues blogPostId)
  where
    sql =  "select ?? from tag \
            inner join blog_post_tag on blog_post_tag.tag_id = tag.id \
            where blog_post_tag.blog_post_id = ?"

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

selectBlogPostsByUser :: UserId -> Handler [(Entity BlogPost, Maybe (Entity Category))]
selectBlogPostsByUser userId = runDB $ rawSql sql (keyToValues userId)
  where
    sql =  "select ??, ?? from blog_post \
            inner join category on blog_post.category_id = category.id \
            where blog_post.author_id = ? \
            order by blog_post.created desc"

selectBlogPostsByTag :: TagId -> Handler [(Entity BlogPost, Maybe (Entity Category))]
selectBlogPostsByTag tagId = runDB $ rawSql sql (keyToValues tagId)
  where
    sql = "select ??, ?? from blog_post \
           left join category on blog_post.category_id = category.id \
           inner join blog_post_tag on blog_post.id = blog_post_tag.blog_post_id \
           where blog_post_tag.tag_id = ? \
           order by blog_post.created desc"
