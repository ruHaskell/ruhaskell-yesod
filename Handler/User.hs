module Handler.User where

import Import
import Helper
import Yesod.Form.Bootstrap3
import Handler.BlogPost (blogPostList)

isAdmin :: Handler Bool
isAdmin = do
    mauth <- maybeAuth
    let Entity _ user = fromMaybe (error "Not authenticated") mauth
    return $ userAdmin user

userForm :: Bool -> Maybe User -> Form User
userForm admin muser = renderBootstrap3 BootstrapBasicForm $ form admin
  where
    form True  = User
        <$> areq identField     (bfs MsgUserIdent)     mident
        <*> areq nameField      (bfs MsgUserName)      mname
        <*> areq textField      (bfs MsgUserAvatarUrl) (userAvatarUrl <$> muser)
        <*> areq checkBoxField  (bfs MsgUserAdmin)     (userAdmin <$> muser)
    form False = User
        <$> maybe (error "No user") pure mident
        <*> areq nameField (bfs MsgUserName) mname
        <*> maybe (error "No user") pure (userAvatarUrl <$> muser)
        <*> maybe (error "No user") pure (userAdmin <$> muser)

    mident     = userIdent <$> muser
    mname      = userName <$> muser
    identField = checkM (isUniq MsgUserIdentAlreadyExists UserIdent mident) textField
    nameField  = checkM (isUniq MsgUserNameAlreadyExists  UserName  mname)  textField

getUsersR :: Handler Html
getUsersR = do
    users <- selectUsers
    defaultLayout $(widgetFile "users/index")

postUsersR :: Handler Html
postUsersR = do
    admin <- isAdmin
    ((res, formWidget), enctype) <- runFormPost $ userForm admin Nothing
    case res of
        FormSuccess user -> do
            _ <- runDB $ insert user
            setMessageI MsgUserWasAdded
            redirect UsersR
        _ -> defaultLayout $(widgetFile "users/new")

getNewUserR :: Handler Html
getNewUserR = do
    admin <- isAdmin
    (formWidget, enctype) <- generateFormPost $ userForm admin Nothing
    defaultLayout $(widgetFile "users/new")

getUserR :: UserId -> Handler Html
getUserR userId = do
    mauth <- maybeAuth
    profile <- runDB $ get404 userId
    blogPosts <- selectBlogPostsByUser userId
    defaultLayout $(widgetFile "users/show")

patchUserR :: UserId -> Handler Html
patchUserR userId = do
    admin <- isAdmin
    user <- runDB $ get404 userId
    ((res, formWidget), enctype) <- runFormPost $ userForm admin (Just user)
    case res of
        FormSuccess user' -> do
            runDB $ replace userId $ user'
            setMessageI MsgUserWasUpdated
            redirect UsersR
        _ -> defaultLayout $(widgetFile "users/edit")

getEditUserR :: UserId -> Handler Html
getEditUserR userId = do
    admin <- isAdmin
    user <- runDB $ get404 userId
    (formWidget, enctype) <- generateFormPost $ userForm admin (Just user)
    defaultLayout $(widgetFile "users/edit")

deleteUserR :: UserId -> Handler Html
deleteUserR userId = do
    runDB $ get404 userId >> delete userId
    redirect UsersR
