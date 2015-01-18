module Settings.Config where

import Prelude
import Language.Haskell.TH.Syntax
import System.Environment         (lookupEnv)
import Yesod.Default.Config2      (configSettingsYml)

configPath :: Q FilePath
configPath = do
    path <- runIO $ lookupEnv "RH_CONFIG_PATH"
    return . maybe configSettingsYml id $ path
