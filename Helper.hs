module Helper where

import Import

-- isUniq :: Text -> EntityField v a -> Maybe a -> a -> Handler (Either Text a)
isUniq errorMessage field mexclude value = do
    count' <- runDB . count $ [field ==. value] ++ exclude
    return $ if count' > 0
             then Left errorMessage
             else Right value
  where
    exclude = maybe [] (\x -> [field !=. x]) mexclude
